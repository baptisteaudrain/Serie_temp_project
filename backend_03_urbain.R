# ==============================================================================
# BACKEND : PREVISIONS 24H O3 URBAIN (SARIMA + GJR-GARCH) - TIME MACHINE
# Modèle : SARIMA(0,1,1)(0,1,1)[24] + gjrGARCH(1,1)
# ==============================================================================

rm(list = ls())

library(dplyr)
library(lubridate)
library(zoo)
library(forecast)
library(rugarch)
library(future.apply)
library(progressr)

# ----------------------------------------------------------------------
# 0. Paramètres généraux
# ----------------------------------------------------------------------
INPUT_FILE   <- "mesure_horaire_view.csv"
OUTPUT_FILE  <- "data_shiny_o3_urbain.csv"
SEUIL_ALERTE <- 180    # µg/m³ O3

# --- CONFIGURATION TIME MACHINE ---
DATE_LIMITE_SIMU <- ymd_hms("2026-01-06 12:00:00")

# Parallélisation
n_cores <- parallel::detectCores() - 1
if (n_cores < 1) n_cores <- 1
plan(multisession, workers = n_cores)
handlers(global = TRUE)
handlers("txtprogressbar")

cat(">>> Mode Time Machine activé : coupure au", as.character(DATE_LIMITE_SIMU), "\n")

if (!file.exists(INPUT_FILE)) stop("Fichier source introuvable.")
df <- read.csv(INPUT_FILE)

# ----------------------------------------------------------------------
# 1. Filtre : O3 / URBAINE + Time Machine
# ----------------------------------------------------------------------
df_traite <- df %>%
  filter(nom_polluant == "O3",
         typologie == "Urbaine") %>%
  mutate(date_fin = ymd_hms(date_fin, quiet = TRUE)) %>%
  filter(!is.na(date_fin)) %>%
  filter(date_fin <= DATE_LIMITE_SIMU)

# Stations actives : au moins une mesure dans les 3 derniers jours avant la coupure
stations_actives <- df_traite %>%
  group_by(nom_station) %>%
  summarise(derniere_mesure = max(date_fin, na.rm = TRUE), .groups = "drop") %>%
  filter(derniere_mesure >= (max(df_traite$date_fin, na.rm = TRUE) - days(3))) %>%
  pull(nom_station)

cat("Stations urbaines actives (O3) au", as.character(DATE_LIMITE_SIMU), ":\n")
print(stations_actives)

if (length(stations_actives) == 0) {
  stop("ERREUR : aucune station urbaine O3 active à cette date limite.")
}

# ----------------------------------------------------------------------
# 2. Fonction de traitement par station (style backend 1)
# ----------------------------------------------------------------------
process_station_worker <- function(station_name, data_full) {
  
  df_s <- data_full %>%
    filter(nom_station == station_name) %>%
    arrange(date_fin)
  
  # Série trop courte -> on ignore
  if (nrow(df_s) < 150) return(NULL)
  
  last_time <- max(df_s$date_fin)
  
  # Grille horaire régulière et interpolation
  grille <- data.frame(date_fin = seq(min(df_s$date_fin), last_time, by = "hour"))
  df_clean <- merge(grille, df_s, by = "date_fin", all.x = TRUE)
  valeurs_impute <- na.approx(df_clean$valeur, rule = 2)
  
  res <- tryCatch({
    # Série temporelle
    ts_data <- ts(valeurs_impute, frequency = 24)
    
    # SARIMA (moyenne) - même esprit que ton second script
    fit_sarima <- Arima(
      ts_data,
      order   = c(0, 1, 1),
      seasonal = list(order = c(0, 1, 1), period = 24)
    )
    
    residus <- residuals(fit_sarima)
    
    # GJR-GARCH(1,1) sur les résidus
    spec_garch_asym <- ugarchspec(
      variance.model = list(
        model = "gjrGARCH",
        garchOrder = c(1, 1)
      ),
      mean.model = list(
        armaOrder = c(0, 0),
        include.mean = FALSE
      ),
      distribution.model = "norm"
    )
    
    fit_garch_asym <- tryCatch(
      ugarchfit(
        spec   = spec_garch_asym,
        data   = residus,
        solver = "solnp"
      ),
      error = function(e) {
        message(" -> Erreur GARCH sur station ", station_name, " : ", e$message)
        return(NULL)
      }
    )
    
    if (is.null(fit_garch_asym)) return(NULL)
    
    # Horizon 24h
    horizon <- 24
    
    prev_sarima <- forecast(fit_sarima, h = horizon)
    pred_mean   <- as.numeric(prev_sarima$mean)
    
    prev_garch  <- ugarchforecast(fit_garch_asym, n.ahead = horizon)
    pred_sigma  <- as.numeric(sigma(prev_garch))
    
    # Bornes 80% et 95%
    vec_max_80 <- pred_mean + 1.28 * pred_sigma
    vec_max_95 <- pred_mean + 1.96 * pred_sigma
    
    # Data frame de sortie (aligné sur ton backend “top”)
    df_out <- data.frame(
      Station    = station_name,
      Polluant   = "O3",
      Typologie  = df_s$typologie[1],
      Commune    = df_s$nom_com[1],
      Lat        = df_s$y_wgs84[1],
      Lon        = df_s$x_wgs84[1],
      Heure_Ref  = last_time,
      Echeance_H = 1:horizon,
      Date_Prevue = last_time + hours(1:horizon),
      Heure_Prediction = format(last_time + hours(1:horizon), "%H:00"),
      
      Pred_Mean   = round(pmax(pred_mean, 0), 2),
      Pred_Max_80 = round(pmax(vec_max_80, 0), 2),
      Pred_Max_95 = round(pmax(vec_max_95, 0), 2)
    ) %>%
      mutate(
        Statut = case_when(
          Pred_Mean   > SEUIL_ALERTE ~ "ALERTE HAUTE",    # la moyenne dépasse
          Pred_Max_80 > SEUIL_ALERTE ~ "ALERTE MOYENNE",  # borne 80% dépasse
          Pred_Max_95 > SEUIL_ALERTE ~ "ALERTE POSSIBLE", # borne 95% dépasse
          TRUE                       ~ "Normal"
        )
      )
    
    return(df_out)
    
  }, error = function(e) {
    message(" -> Erreur globale station ", station_name, " : ", e$message)
    return(NULL)
  })
  
  return(res)
}

# ----------------------------------------------------------------------
# 3. Exécution en parallèle
# ----------------------------------------------------------------------
with_progress({
  p <- progressor(along = stations_actives)
  resultats_list <- future_lapply(stations_actives, function(st) {
    res <- process_station_worker(st, df_traite)
    p(sprintf("Station : %s", st))
    return(res)
  }, future.seed = TRUE)
})

resultats <- bind_rows(resultats_list)

cat("\nNombre de stations effectivement traitées :",
    ifelse(nrow(resultats) > 0, length(unique(resultats$Station)), 0), "\n")

# ----------------------------------------------------------------------
# 4. Export CSV pour Shiny
# ----------------------------------------------------------------------
write.csv(resultats, OUTPUT_FILE, row.names = FALSE)
cat("Fichier de sortie écrit :", OUTPUT_FILE, "\n")
