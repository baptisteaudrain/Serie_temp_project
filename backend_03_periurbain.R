# ==============================================================================
# BACKEND 3 : O3 (Périurbain) - TIME MACHINE (2026-01-06 12:00:00)
# Modèle : SARIMA(1,1,0)(0,1,1)[24] + GARCH(1,1)
# ==============================================================================
library(dplyr)
library(lubridate)
library(zoo)
library(forecast)
library(rugarch)
library(future.apply) 
library(progressr)    

INPUT_FILE   <- "mesure_horaire_view.csv"
OUTPUT_FILE  <- "data_shiny_o3_periurbain.csv"
SEUIL_ALERTE <- 180 

# --- CONFIGURATION TIME MACHINE ---
# On force le script à croire qu'on est à cette date précise.
# Toutes les données futures sont ignorées.
DATE_LIMITE_SIMU <- ymd_hms("2026-01-06 12:00:00")

# 1. CONFIGURATION PARALLÈLE
# --------------------------
n_cores <- parallel::detectCores() - 1
if(n_cores < 1) n_cores <- 1
plan(multisession, workers = n_cores)
handlers(global = TRUE)
handlers("txtprogressbar") 

print(paste(">>> Mode Time Machine activé : Coupure au", DATE_LIMITE_SIMU))

if (!file.exists(INPUT_FILE)) stop("Fichier source introuvable.")
df <- read.csv(INPUT_FILE)

# 2. PRÉPARATION ET FILTRAGE TEMPOREL
# -----------------------------------
df_traite <- df %>%
  filter(nom_polluant == "O3") %>%
  filter(grepl("Périurbain", typologie, ignore.case = TRUE)) %>% 
  mutate(date_fin = ymd_hms(date_fin, quiet = TRUE)) %>%
  filter(!is.na(date_fin)) %>%
  
  # === LA LIGNE MAGIQUE ===
  # On coupe tout ce qui dépasse la date limite
  filter(date_fin <= DATE_LIMITE_SIMU) 
# ========================

stations_actives <- df_traite %>%
  group_by(nom_station) %>%
  summarise(derniere_mesure = max(date_fin, na.rm = TRUE)) %>%
  # On vérifie que la station était active juste avant notre date limite (3 jours max)
  filter(derniere_mesure >= (max(df_traite$date_fin, na.rm=T) - days(3))) %>%
  pull(nom_station)

print(paste("Nombre de stations O3 actives au", DATE_LIMITE_SIMU, ":", length(stations_actives)))

if (length(stations_actives) == 0) {
  stop("ERREUR : Aucune station trouvée à cette date limite. Vérifiez que votre CSV contient des données antérieures à Janvier 2026.")
}

# 3. FONCTION DE TRAITEMENT
# -------------------------
process_station_worker <- function(station_name, data_full) {
  
  df_s <- data_full %>% filter(nom_station == station_name) %>% arrange(date_fin)
  if (nrow(df_s) < 150) return(NULL) 
  
  last_time <- max(df_s$date_fin) # Ce sera forcément <= 2026-01-06 12:00:00
  
  grille <- data.frame(date_fin = seq(min(df_s$date_fin), last_time, by = "hour"))
  df_clean <- merge(grille, df_s, by = "date_fin", all.x = TRUE)
  valeurs_impute <- na.approx(df_clean$valeur, rule = 2)
  
  res <- tryCatch({
    ts_data <- ts(valeurs_impute, frequency = 24)
    
    # Modèle validé : SARIMA(1,1,0)
    fit_sarima <- Arima(ts_data, order = c(1, 1, 0), 
                        seasonal = list(order = c(0, 1, 1), period = 24))
    
    residus <- residuals(fit_sarima)
    
    # GARCH
    spec_garch <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                             mean.model = list(armaOrder = c(0, 0), include.mean = FALSE))
    fit_garch <- ugarchfit(spec = spec_garch, data = residus, solver = "solnp")
    
    horizon <- 48
    pred_mean <- as.numeric(forecast(fit_sarima, h = horizon)$mean)
    pred_sigma <- as.numeric(sigma(ugarchforecast(fit_garch, n.ahead = horizon)))
    
    
    vec_max_80 <- pred_mean + 1.28 * pred_sigma
    vec_max_95 <- pred_mean + 1.96 * pred_sigma
    
    data.frame(
      Station = station_name,
      Polluant = "O3", # <-- CHANGEZ "O3" par "PM2.5" ou "NO2" selon le script !
      Typologie = df_s$typologie[1],
      Lat = df_s$y_wgs84[1],
      Lon = df_s$x_wgs84[1],
      Heure_Ref = last_time,
      Echeance_H = 1:horizon,
      Date_Prevue = last_time + hours(1:horizon),
      
      # 1. La Prévision Moyenne
      Pred_Mean = round(pmax(pred_mean, 0), 2),
      
      # 2. Les Bornes Hautes (Risque)
      Pred_Max_80 = round(pmax(vec_max_80, 0), 2), # Borne probable
      Pred_Max_95 = round(pmax(vec_max_95, 0), 2)  # Borne de sécurité (Pire cas)
    ) %>%
      mutate(Statut = case_when(
        # NIVEAU 3 : ALERTE ROUGE (La moyenne elle-même dépasse le seuil)
        Pred_Mean > SEUIL_ALERTE ~ "ALERTE HAUTE",
        
        # NIVEAU 2 : ALERTE ORANGE (Le haut de l'intervalle 80% dépasse)
        # Signifie : "Il y a une probabilité significative (20%+) de dépasser"
        Pred_Max_80 > SEUIL_ALERTE ~ "ALERTE MOYENNE",
        
        # NIVEAU 1 : ALERTE JAUNE (Le haut de l'intervalle 95% dépasse)
        # Signifie : "Ce n'est pas le plus probable, mais le risque existe (Scénario du pire)"
        Pred_Max_95 > SEUIL_ALERTE ~ "ALERTE POSSIBLE",
        
        # NIVEAU 0 : Tout va bien
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