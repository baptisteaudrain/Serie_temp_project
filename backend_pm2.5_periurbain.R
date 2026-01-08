# ==============================================================================
# SCRIPT BACKEND : GÉNÉRATION PRÉVISIONS 24H AVEC HORODATAGE
# ==============================================================================
library(dplyr)
library(lubridate)
library(zoo)        
library(forecast)   
library(rugarch)    

INPUT_FILE <- "mesure_horaire_view.csv"
OUTPUT_FILE <- "data_shiny_pm25_periurbain.csv"
SEUIL_ALERTE <- 25 

if (!file.exists(INPUT_FILE)) stop("Fichier source introuvable.")
df <- read.csv(INPUT_FILE)

df_traite <- df %>%
  filter(nom_polluant == "PM2.5",
         typologie %in% c("Périurbaine", "Rurale proche Zone Urbaine")) %>%
  mutate(date_fin = ymd_hms(date_fin, quiet = TRUE)) %>%
  filter(!is.na(date_fin))

stations_actives <- df_traite %>%
  group_by(nom_station) %>%
  summarise(derniere_mesure = max(date_fin, na.rm = TRUE)) %>%
  filter(derniere_mesure >= (max(df_traite$date_fin, na.rm=T) - days(3))) %>%
  pull(nom_station)

process_station <- function(station_name) {
  df_s <- df_traite %>% filter(nom_station == station_name) %>% arrange(date_fin)
  if (nrow(df_s) < 500) return(NULL)
  
  last_time <- max(df_s$date_fin)
  grille <- data.frame(date_fin = seq(min(df_s$date_fin), last_time, by = "hour"))
  df_clean <- merge(grille, df_s, by = "date_fin", all.x = TRUE)
  valeurs_impute <- na.approx(df_clean$valeur, rule = 2)
  
  res <- tryCatch({
    ts_data <- ts(valeurs_impute, frequency = 24)
    fit_sarima <- Arima(ts_data, order = c(2, 0, 0), seasonal = list(order = c(0, 1, 1), period = 24))
    residus <- residuals(fit_sarima)
    
    spec_garch <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                             mean.model = list(armaOrder = c(0, 0), include.mean = FALSE))
    fit_garch <- ugarchfit(spec = spec_garch, data = residus, solver = "solnp")
    
    pred_mean <- as.numeric(forecast(fit_sarima, h = 24)$mean)
    pred_sigma <- as.numeric(sigma(ugarchforecast(fit_garch, n.ahead = 24)))
    
    data.frame(
      Station = station_name,
      Commune = df_s$nom_com[1],
      Typologie = "Périurbain",
      Lat = df_s$y_wgs84[1],
      Lon = df_s$x_wgs84[1],
      Heure_Ref = last_time,
      Echeance_H = 1:24,
      # Cette colonne sera affichée dans le tableau Shiny
      Heure_Prediction = format(last_time + hours(1:24), "%H:00"),
      Date_Prevue = last_time + hours(1:24),
      Pred_PM25 = round(pred_mean, 2),
      Risque_Max = round(pred_mean + 1.96 * pred_sigma, 2)
    ) %>%
      mutate(Statut = ifelse(Risque_Max > SEUIL_ALERTE, "ALERTE RISQUE", "Normal"))
  }, error = function(e) NULL)
  
  return(res)
}

resultats <- bind_rows(lapply(stations_actives, process_station))
write.csv(resultats, OUTPUT_FILE, row.names = FALSE)