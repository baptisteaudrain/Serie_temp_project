# ==============================================================================
# SCRIPT BACKEND : PREVISIONS 24H O3 URBAIN (SARIMA + GJR-GARCH)
# ==============================================================================

rm(list = ls())

library(dplyr)
library(lubridate)
library(zoo)
library(forecast)
library(rugarch)
library(future.apply)
# install.packages("progressr")
library(progressr)    

# ----------------------------------------------------------------------
# 0. Paramètres généraux
# ----------------------------------------------------------------------
INPUT_FILE   <- "mesure_horaire_view.csv"
OUTPUT_FILE  <- "data_shiny_o3_urbain.csv"
SEUIL_ALERTE <- 180    # µg/m3 O3 (par ex. seuil d'information, à adapter)


# --- CONFIGURATION TIME MACHINE ---
# On fige le temps pour la démo
DATE_LIMITE_SIMU <- ymd_hms("2026-01-06 12:00:00")


if (!file.exists(INPUT_FILE)) stop("Fichier source introuvable.")
df <- read.csv(INPUT_FILE)

# ----------------------------------------------------------------------
# 1. Filtre : O3 / URBAINE
# ----------------------------------------------------------------------
df_traite <- df %>%
  filter(nom_polluant == "O3",
         typologie == "Urbaine") %>%
  mutate(date_fin = ymd_hms(date_fin, quiet = TRUE)) %>%
  filter(!is.na(date_fin))
  # === LA TIME MACHINE ===
  filter(date_fin <= DATE_LIMITE_SIMU)

# Stations actives : au moins une mesure dans les 3 derniers jours
stations_actives <- df_traite %>%
  group_by(nom_station) %>%
  summarise(derniere_mesure = max(date_fin, na.rm = TRUE), .groups = "drop") %>%
  filter(derniere_mesure >= (max(df_traite$date_fin, na.rm = TRUE) - days(3))) %>%
  pull(nom_station)

print("Stations urbaines actives (O3) :")
print(stations_actives)

# ----------------------------------------------------------------------
# 2. Fonction de traitement par station
# ----------------------------------------------------------------------
process_station <- function(station_name) {
  
  cat("\n-----------------------------\n")
  cat("Traitement station :", station_name, "\n")
  
  df_s <- df_traite %>%
    filter(nom_station == station_name) %>%
    arrange(date_fin)
  
  # On écarte les séries trop courtes
  if (nrow(df_s) < 500) {
    cat(" -> Série trop courte, station ignorée.\n")
    return(NULL)
  }
  
  # Option : limiter l'historique pour accélérer
  # df_s <- df_s %>% filter(date_fin >= (max(date_fin) - days(365)))
  
  last_time <- max(df_s$date_fin)
  
  # Grille horaire régulière et interpolation
  grille <- data.frame(
    date_fin = seq(min(df_s$date_fin), last_time, by = "hour")
  )
  
  df_clean <- merge(grille, df_s, by = "date_fin", all.x = TRUE)
  valeurs_impute <- na.approx(df_clean$valeur, rule = 2)
  
  res <- tryCatch({
    
    # 2.1 Série temporelle
    ts_data <- ts(valeurs_impute, frequency = 24)
    
    # 2.2 Modèle SARIMA (moyenne)
    # Ici, on réutilise la structure ARIMA(1,0,2)(1,1,1)[24] comme point de départ.
    # Tu peux la remplacer par la meilleure structure trouvée pour O3 dans ton script sandbox.
    fit_sarima <- Arima(
      ts_data,
      order   = c(0, 1, 1),
      seasonal = list(order = c(0, 1, 1), period = 24)
    )
    
    residus <- residuals(fit_sarima)
    
    # 2.3 Modèle GARCH asymétrique sur les résidus : gjrGARCH(1,1)
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
        cat(" -> Erreur GARCH sur station", station_name, ":", e$message, "\n")
        return(NULL)
      }
    )
    
    if (is.null(fit_garch_asym)) return(NULL)
    
    # 2.4 Prévisions 24h : moyenne (SARIMA) + volatilité (GARCH)
    prev_sarima <- forecast(fit_sarima, h = 24)
    pred_mean   <- as.numeric(prev_sarima$mean)
    
    prev_garch <- tryCatch(
      ugarchforecast(fit_garch_asym, n.ahead = 24),
      error = function(e) {
        cat(" -> Erreur ugarchforecast sur station", station_name, ":", e$message, "\n")
        return(NULL)
      }
    )
    
    if (is.null(prev_garch)) return(NULL)
    
    pred_sigma <- as.numeric(sigma(prev_garch))
    
    # 2.5 Construction du data.frame de sortie
    df_out <- data.frame(
      Station    = station_name,
      Commune    = df_s$nom_com[1],
      Typologie  = "Urbaine",
      Lat        = df_s$y_wgs84[1],
      Lon        = df_s$x_wgs84[1],
      Heure_Ref  = last_time,
      Echeance_H = 1:24,
      Heure_Prediction = format(last_time + hours(1:24), "%H:00"),
      Date_Prevue      = last_time + hours(1:24),
      Pred_O3          = round(pred_mean, 2),
      Risque_Max       = round(pred_mean + 1.96 * pred_sigma, 2)
    )
    
    df_out <- df_out %>%
      mutate(
        Statut = ifelse(Risque_Max > SEUIL_ALERTE,
                        "ALERTE RISQUE", "Normal")
      )
    
    cat(" -> Station traitée OK.\n")
    
    return(df_out)
    
  }, error = function(e) {
    cat(" -> Erreur globale station", station_name, ":", e$message, "\n")
    return(NULL)
  })
  
  return(res)
}

# ----------------------------------------------------------------------
# 3. Boucle sur toutes les stations urbaines actives
# ----------------------------------------------------------------------
resultats_list <- lapply(stations_actives, process_station)
resultats <- bind_rows(resultats_list)

cat("\nNombre de stations effectivement traitées :", nrow(resultats) / 24, "\n")

# ----------------------------------------------------------------------
# 4. Export vers fichier CSV pour Shiny
# ----------------------------------------------------------------------
write.csv(resultats, OUTPUT_FILE, row.names = FALSE)
cat("Fichier de sortie écrit :", OUTPUT_FILE, "\n")
