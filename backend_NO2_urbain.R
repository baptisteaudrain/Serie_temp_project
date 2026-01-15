# ==============================================================================
# BACKEND 2 : NO2 - VERSION PARALLÈLE (Précision maximale)
# ==============================================================================

rm(list = ls())
library(dplyr)
library(lubridate)
library(zoo)
library(forecast)
library(rugarch)
library(future.apply)
library(progressr)

INPUT_FILE   <- "mesure_horaire_view.csv"
OUTPUT_FILE  <- "data_shiny_no2_urbain.csv"
SEUIL_ALERTE <- 200  # NO2

# --- CONFIGURATION TIME MACHINE ---
DATE_LIMITE_SIMU <- ymd_hms("2026-01-06 12:00:00")

# 1. CONFIGURATION PARALLÈLE
n_cores <- parallel::detectCores() - 1
if (n_cores < 1) n_cores <- 1
plan(multisession, workers = n_cores)
handlers(global = TRUE)
handlers("txtprogressbar")

cat(">>> Mode PARALLÈLE activé sur", n_cores, "cœurs.\n")
cat(">>> Méthode : SARIMA (2,1,2)(0,1,1)[24] + sGARCH(1,1)\n")

if (!file.exists(INPUT_FILE)) stop("Fichier source introuvable.")
df <- read.csv(INPUT_FILE)

# 2. PRÉPARATION
df_traite <- df %>%
  filter(nom_polluant == "NO2",
         typologie %in% c("Urbaine")) %>%
  mutate(date_fin = ymd_hms(date_fin, quiet = TRUE)) %>%
  filter(!is.na(date_fin)) %>%
  # TIME MACHINE
  filter(date_fin <= DATE_LIMITE_SIMU)

stations_actives <- df_traite %>%
  group_by(nom_station) %>%
  summarise(derniere_mesure = max(date_fin, na.rm = TRUE), .groups = "drop") %>%
  filter(derniere_mesure >= (max(df_traite$date_fin, na.rm = TRUE) - days(3))) %>%
  pull(nom_station)

cat("Stations NO2 urbaines actives au", as.character(DATE_LIMITE_SIMU), ":\n")
print(stations_actives)

if (length(stations_actives) == 0) {
  stop("ERREUR : aucune station NO2 urbaine active à cette date limite.")
}

# 3. FONCTION DE TRAITEMENT
process_station_worker <- function(station_name, data_full) {
  
  df_s <- data_full %>%
    filter(nom_station == station_name) %>%
    arrange(date_fin)
  
  if (nrow(df_s) < 150) return(NULL)
  
  last_time <- max(df_s$date_fin)
  
  # Grille horaire et interpolation
  grille <- data.frame(date_fin = seq(min(df_s$date_fin), last_time, by = "hour"))
  df_clean <- merge(grille, df_s, by = "date_fin", all.x = TRUE)
  valeurs_impute <- na.approx(df_clean$valeur, rule = 2)
  
  res <- tryCatch({
    
    ts_data <- ts(valeurs_impute, frequency = 24)
    
    # SARIMA plus précis
    fit_sarima <- Arima(
      ts_data,
      order   = c(2, 1, 2),
      seasonal = list(order = c(0, 1, 1), period = 24)
    )
    
    residus <- residuals(fit_sarima)
    
    # sGARCH(1,1)
    spec_garch <- ugarchspec(
      variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
      mean.model     = list(armaOrder = c(0, 0), include.mean = FALSE)
    )
    
    fit_garch <- ugarchfit(
      spec   = spec_garch,
      data   = residus,
      solver = "solnp"
    )
    
    horizon    <- 48
    prev_sarima <- forecast(fit_sarima, h = horizon)
    pred_mean   <- as.numeric(prev_sarima$mean)
    
    prev_garch <- ugarchforecast(fit_garch, n.ahead = horizon)
    pred_sigma <- as.numeric(sigma(prev_garch))
    
    # Bornes 80 % et 95 %
    vec_max_80 <- pred_mean + 1.2816 * pred_sigma
    vec_max_95 <- pred_mean + 1.96   * pred_sigma
    
    df_out <- data.frame(
      Station    = station_name,
      Polluant   = "NO2",
      Typologie  = df_s$typologie[1],
      Commune    = df_s$nom_com[1],
      Lat        = df_s$y_wgs84[1],
      Lon        = df_s$x_wgs84[1],
      Heure_Ref  = last_time,
      Echeance_H = 1:horizon,
      Date_Prevue = last_time + hours(1:horizon),
      
      Pred_Mean   = round(pmax(pred_mean,   0), 2),
      Pred_Max_80 = round(pmax(vec_max_80,  0), 2),
      Pred_Max_95 = round(pmax(vec_max_95,  0), 2)
    ) %>%
      mutate(
        Statut = case_when(
          Pred_Mean   > SEUIL_ALERTE ~ "ALERTE HAUTE",    # la moyenne dépasse
          Pred_Max_80 > SEUIL_ALERTE ~ "ALERTE MOYENNE",  # borne 80 % dépasse
          Pred_Max_95 > SEUIL_ALERTE ~ "ALERTE POSSIBLE", # borne 95 % dépasse
          TRUE                       ~ "Normal"
        )
      )
    
    return(df_out)
    
  }, error = function(e) {
    message(" -> Erreur station ", station_name, " : ", e$message)
    return(NULL)
  })
  
  return(res)
}

# 4. EXÉCUTION AVEC BARRE DE PROGRESSION
n_total <- length(stations_actives)
cat(">>> Démarrage du traitement pour", n_total, "stations...\n")

with_progress({
  p <- progressor(along = stations_actives)
  
  resultats_list <- future_lapply(stations_actives, function(st) {
    res <- process_station_worker(st, df_traite)
    p(sprintf("Station : %s", st))
    return(res)
  }, future.seed = TRUE)
})

cat("Fusion des résultats...\n")
resultats <- bind_rows(resultats_list)

write.csv(resultats, OUTPUT_FILE, row.names = FALSE)
cat(">>> Terminé ! Fichier :", OUTPUT_FILE, "\n")
