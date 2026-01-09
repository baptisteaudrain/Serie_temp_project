# ==============================================================================
# BACKEND 2 : NO2 - VERSION PARALLÈLE (7 Cœurs) - PRÉCISION MAXIMALE
# ==============================================================================
library(dplyr)
library(lubridate)
library(zoo)
library(forecast)
library(rugarch)
library(future.apply) 
library(progressr)    

INPUT_FILE  <- "mesure_horaire_view.csv"
OUTPUT_FILE <- "data_shiny_no2_periubain.csv"
SEUIL_ALERTE <- 200

# 1. CONFIGURATION DU PARALLÉLISME
# --------------------------------
n_cores <- parallel::detectCores() - 1
if(n_cores < 1) n_cores <- 1

# On configure les "workers" (les cœurs)
plan(multisession, workers = n_cores)

# Configuration de la barre de progression pour qu'elle soit visible
handlers(global = TRUE)
handlers("txtprogressbar") # Affiche une barre [=====>   ] dans la console

print(paste(">>> Mode PARALLÈLE activé sur", n_cores, "cœurs."))
print(">>> Méthode : SARIMA (Standard) + GARCH (solnp)")

if (!file.exists(INPUT_FILE)) stop("Fichier source introuvable.")
df <- read.csv(INPUT_FILE)

# 2. PRÉPARATION
# --------------
df_traite <- df %>%
  filter(nom_polluant == "NO2",
         typologie %in% c("Urbaine", "Périurbaine", "Rurale proche Zone Urbaine")) %>%
  mutate(date_fin = ymd_hms(date_fin, quiet = TRUE)) %>%
  filter(!is.na(date_fin))

stations_actives <- df_traite %>%
  group_by(nom_station) %>%
  summarise(derniere_mesure = max(date_fin, na.rm = TRUE)) %>%
  filter(derniere_mesure >= (max(df_traite$date_fin, na.rm=T) - days(3))) %>%
  pull(nom_station)

# 3. FONCTION DE TRAITEMENT (Précision Maximale)
# ----------------------------------------------
process_station_worker <- function(station_name, data_full) {
  
  df_s <- data_full %>% filter(nom_station == station_name) %>% arrange(date_fin)
  if (nrow(df_s) < 150) return(NULL) 
  
  last_time <- max(df_s$date_fin)
  grille <- data.frame(date_fin = seq(min(df_s$date_fin), last_time, by = "hour"))
  df_clean <- merge(grille, df_s, by = "date_fin", all.x = TRUE)
  valeurs_impute <- na.approx(df_clean$valeur, rule = 2)
  
  res <- tryCatch({
    ts_data <- ts(valeurs_impute, frequency = 24)
    
    # RETOUR AU STANDARD (Plus lent mais plus précis)
    # On enlève method="CSS" pour laisser Arima faire son optimisation complète
    fit_sarima <- Arima(ts_data, order = c(2, 1, 2), 
                        seasonal = list(order = c(0, 1, 1), period = 24))
    
    residus <- residuals(fit_sarima)
    
    # RETOUR AU SOLVER STANDARD (solnp)
    spec_garch <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                             mean.model = list(armaOrder = c(0, 0), include.mean = FALSE))
    fit_garch <- ugarchfit(spec = spec_garch, data = residus, solver = "solnp")
    
    horizon <- 48
    pred_mean <- as.numeric(forecast(fit_sarima, h = horizon)$mean)
    pred_sigma <- as.numeric(sigma(ugarchforecast(fit_garch, n.ahead = horizon)))
    
    data.frame(
      Station = station_name,
      Polluant = "NO2",
      Typologie = df_s$typologie[1],
      Lat = df_s$y_wgs84[1],
      Lon = df_s$x_wgs84[1],
      Heure_Ref = last_time,
      Echeance_H = 1:horizon,
      Date_Prevue = last_time + hours(1:horizon),
      Pred_Mean = round(pmax(pred_mean, 0), 2),
      Pred_Max  = round(pmax(pred_mean + 1.96 * pred_sigma, 0), 2)
    ) %>%
      mutate(Statut = ifelse(Pred_Max > SEUIL_ALERTE, "ALERTE", "Normal"))
    
  }, error = function(e) { return(NULL) })
  
  return(res)
}

# 4. EXÉCUTION AVEC BARRE DE PROGRESSION
# ======================================
n_total <- length(stations_actives)
print(paste(">>> Démarrage du traitement pour", n_total, "stations..."))

with_progress({
  
  # On initialise la barre sur le nombre total de stations
  p <- progressor(along = stations_actives)
  
  resultats_list <- future_lapply(stations_actives, function(st) {
    
    # Calcul
    res <- process_station_worker(st, df_traite)
    
    # Mise à jour de la barre : On envoie un signal pour dire "1 de fait !"
    # sprintf permet d'afficher le nom de la station en cours de traitement dans la barre
    p(sprintf("Station : %s", st))
    
    return(res)
  }, future.seed = TRUE)
})

print("Fusion des résultats...")
resultats <- bind_rows(resultats_list)

write.csv(resultats, OUTPUT_FILE, row.names = FALSE)
print(paste(">>> Terminé ! Fichier :", OUTPUT_FILE))