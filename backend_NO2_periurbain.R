# ==============================================================================
# BACKEND 2 : NO2 - TIME MACHINE (2026-01-06 12:00:00)
# Modèle : SARIMA(2,1,2)(0,1,1)[24] + GARCH(1,1) (Selon Validation Image)
# ==============================================================================
library(dplyr)
library(lubridate)
library(zoo)
library(forecast)
library(rugarch)
library(future.apply) 
library(progressr)    

INPUT_FILE  <- "mesure_horaire_view.csv"
OUTPUT_FILE <- "data_shiny_no2_periurbain.csv"
SEUIL_ALERTE <- 200 # Seuil horaire (Europe)

# --- CONFIGURATION TIME MACHINE ---
# On fige le temps pour la démo
DATE_LIMITE_SIMU <- ymd_hms("2026-01-06 12:00:00")

# 1. CONFIGURATION DU PARALLÉLISME
# --------------------------------
n_cores <- parallel::detectCores() - 1
if(n_cores < 1) n_cores <- 1

plan(multisession, workers = n_cores)

# Configuration de la barre de progression
handlers(global = TRUE)
handlers("txtprogressbar") 

print(paste(">>> Mode Time Machine activé : Coupure au", DATE_LIMITE_SIMU))
print(">>> Méthode : SARIMA(2,1,2) + GARCH")

if (!file.exists(INPUT_FILE)) stop("Fichier source introuvable.")
df <- read.csv(INPUT_FILE)

# 2. PRÉPARATION
# --------------
# Pour le NO2, on garde Urbain + Périurbain + Rural proche (Trafic impactant)
df_traite <- df %>%
  filter(nom_polluant == "NO2") %>%
  filter(typologie %in% c("Périurbaine", "Rurale proche Zone Urbaine")) %>%
  mutate(date_fin = ymd_hms(date_fin, quiet = TRUE)) %>%
  filter(!is.na(date_fin)) %>%
  
  # === LA TIME MACHINE ===
  filter(date_fin <= DATE_LIMITE_SIMU)
# =======================

stations_actives <- df_traite %>%
  group_by(nom_station) %>%
  summarise(derniere_mesure = max(date_fin, na.rm = TRUE)) %>%
  filter(derniere_mesure >= (max(df_traite$date_fin, na.rm=T) - days(3))) %>%
  pull(nom_station)

print(paste("Nombre de stations NO2 actives au", DATE_LIMITE_SIMU, ":", length(stations_actives)))

if (length(stations_actives) == 0) stop("ERREUR : Aucune station NO2 trouvée à cette date limite.")

# 3. FONCTION DE TRAITEMENT
# -------------------------
process_station_worker <- function(station_name, data_full) {
  
  df_s <- data_full %>% filter(nom_station == station_name) %>% arrange(date_fin)
  if (nrow(df_s) < 150) return(NULL) 
  
  last_time <- max(df_s$date_fin)
  grille <- data.frame(date_fin = seq(min(df_s$date_fin), last_time, by = "hour"))
  df_clean <- merge(grille, df_s, by = "date_fin", all.x = TRUE)
  valeurs_impute <- na.approx(df_clean$valeur, rule = 2)
  
  res <- tryCatch({
    ts_data <- ts(valeurs_impute, frequency = 24)
    
    # --- MODÈLE NO2 VALIDÉ SUR PHOTO ---
    # SARIMA(2,1,2)(0,1,1)[24]
    fit_sarima <- Arima(ts_data, order = c(2, 1, 2), 
                        seasonal = list(order = c(0, 1, 1), period = 24))
    
    residus <- residuals(fit_sarima)
    
    # GARCH(1,1) Standard (Solver Hybrid pour robustesse si besoin, sinon solnp)
    spec_garch <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                             mean.model = list(armaOrder = c(0, 0), include.mean = FALSE))
    
    # On utilise 'hybrid' ou 'solnp'. 'hybrid' est souvent plus sûr pour NO2 complexe
    fit_garch <- ugarchfit(spec = spec_garch, data = residus, solver = "hybrid") 
    
    horizon <- 48
    pred_mean <- as.numeric(forecast(fit_sarima, h = horizon)$mean)
    pred_sigma <- as.numeric(sigma(ugarchforecast(fit_garch, n.ahead = horizon)))
    
    # Calcul des bornes de risque
    vec_max_80 <- pred_mean + 1.28 * pred_sigma
    vec_max_95 <- pred_mean + 1.96 * pred_sigma
    
    data.frame(
      Station = station_name,
      Polluant = "NO2",
      Typologie = df_s$typologie[1],
      Lat = df_s$y_wgs84[1],
      Lon = df_s$x_wgs84[1],
      Heure_Ref = last_time,
      Echeance_H = 1:horizon,
      Date_Prevue = last_time + hours(1:horizon),
      
      # Colonnes standardisées
      Pred_Mean = round(pmax(pred_mean, 0), 2),
      Pred_Max_80 = round(pmax(vec_max_80, 0), 2),
      Pred_Max_95 = round(pmax(vec_max_95, 0), 2)
    ) %>%
      mutate(Statut = case_when(
        Pred_Mean > SEUIL_ALERTE ~ "ALERTE HAUTE",
        Pred_Max_80 > SEUIL_ALERTE ~ "ALERTE MOYENNE",
        Pred_Max_95 > SEUIL_ALERTE ~ "ALERTE POSSIBLE",
        TRUE ~ "Normal"
      ))
    
  }, error = function(e) { return(NULL) })
  
  return(res)
}

# 4. EXÉCUTION
# ============
with_progress({
  p <- progressor(along = stations_actives)
  resultats_list <- future_lapply(stations_actives, function(st) {
    res <- process_station_worker(st, df_traite)
    p(sprintf("Station : %s", st))
    return(res)
  }, future.seed = TRUE)
})

print("Fusion des résultats...")
resultats <- bind_rows(resultats_list)

write.csv(resultats, OUTPUT_FILE, row.names = FALSE)
print(paste(">>> Terminé ! Fichier :", OUTPUT_FILE))