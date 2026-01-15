library(dplyr)
library(tidyr)
library(lubridate)

build_skeleton_48h <- function(df_rural, polluant, horizon = 48,
                               heure_ref = "2026-01-06 11:00:00",
                               tz = "UTC") {
  
  heure_ref <- as.POSIXct(heure_ref, tz = tz)
  
  # 1) villes qui existent pour ce polluant + coordonnées
  stations <- df_rural %>%
    filter(nom_polluant == polluant) %>%
    group_by(nom_com) %>%
    summarise(
      Lon = mean(x_wgs84, na.rm = TRUE),
      Lat = mean(y_wgs84, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(is.finite(Lat), is.finite(Lon)) %>%
    arrange(nom_com) %>%
    rename(Station = nom_com)
  
  if (nrow(stations) == 0) {
    stop(paste("Aucune station trouvée pour le polluant =", polluant))
  }
  
  # 2) expansion 48h + ajout Heure_Ref et Date_Prevue
  skeleton <- stations %>%
    tidyr::crossing(Echeance_H = 1:horizon) %>%
    mutate(
      Polluant   = polluant,
      Typologie  = "Rurale",
      Heure_Ref  = heure_ref,
      Date_Prevue = heure_ref + hours(Echeance_H)
    ) %>%
    select(Station, Polluant, Typologie, Lat, Lon, Heure_Ref, Echeance_H, Date_Prevue)
  
  skeleton
}

# Exemple (avec PM2.5 comme tu utilises maintenant)
polluants <- c("NO2", "O3", "PM2.5")

skeletons <- lapply(polluants, function(p) build_skeleton_48h(
  df_rural, p,
  horizon = 48,
  heure_ref = "2026-01-06 11:00:00",
  tz = "UTC"
))
names(skeletons) <- polluants

sk_NO2  <- skeletons[["NO2"]]
sk_O3   <- skeletons[["O3"]]
sk_PM25 <- skeletons[["PM2.5"]]

# Checks rapides
tail(sk_NO2, 5)
sk_NO2 %>% filter(Echeance_H %in% c(1,3,48)) %>% head(10)
library(dplyr)
library(forecast)
library(rugarch)

# Unscale helper (si ton SARIMA est sur série standardisée)
unscale_vec <- function(x_sc, mu, sd) x_sc * sd + mu

add_preds_to_skeleton <- function(skeleton_df, pol, models_final, horizon = 48,
                                  use_unscale = TRUE,
                                  garch_resid_scale = TRUE) {
  
  # ---- récupérer les modèles
  sar_fit <- models_final[[pol]]$sarima_fit
  g_fit   <- models_final[[pol]]$garch_fit
  
  # ---- 1) Pred mean via SARIMA
  fc <- forecast::forecast(sar_fit, h = horizon)
  pred_mean_sc <- as.numeric(fc$mean)
  
  if (use_unscale) {
    mu0 <- models_final[[pol]]$sarima_scale$scale_mu
    sd0 <- models_final[[pol]]$sarima_scale$scale_sd
    pred_mean <- unscale_vec(pred_mean_sc, mu0, sd0)
  } else {
    pred_mean <- pred_mean_sc
  }
  
  # ---- 2) Pred sigma via GARCH (forecast de sigma)
  # sigma() renvoie la volatilité de la série utilisée pour fitter le GARCH.
  pred_sigma <- as.numeric(sigma(ugarchforecast(g_fit, n.ahead = horizon)))
  
  # Si ton GARCH a été ajusté sur des résidus "rescalés",
  # tu dois remettre l'échelle d'origine.
  # Dans nos échanges précédents, tu avais un facteur resid_sd.
  if (garch_resid_scale && !is.null(models_final[[pol]]$garch_meta$resid_sd)) {
    pred_sigma <- pred_sigma * models_final[[pol]]$garch_meta$resid_sd
  }
  
  # ---- 3) Max bands (comme tu l’as demandé)
  vec_max_80 <- pred_mean + 1.28 * pred_sigma
  vec_max_95 <- pred_mean + 1.96 * pred_sigma
  
  # ---- 4) table prédictions par échéance
  pred_tbl <- tibble(
    Echeance_H = 1:horizon,
    Pred_Mean = pred_mean,
    Pred_Max_80 = vec_max_80,
    Pred_Max_95 = vec_max_95
  )
  
  # ---- 5) join sur le squelette (réplique sur toutes les stations)
  skeleton_df %>%
    left_join(pred_tbl, by = "Echeance_H") %>%
    # optionnel : forcer l'ordre des colonnes (ajout à droite)
    select(everything())
}

# ============================================================
# EXEMPLE d'utilisation (avec tes squelettes déjà construits)
# ============================================================

# sk_NO2  <- build_skeleton_48h(df_rural, "NO2",  horizon = 48, ...)
# sk_O3   <- build_skeleton_48h(df_rural, "O3",   horizon = 48, ...)
# sk_PM25 <- build_skeleton_48h(df_rural, "PM2.5",horizon = 48, ...)

sk_NO2_pred  <- add_preds_to_skeleton(sk_NO2,  "NO2",  models_final, horizon = 48)
sk_O3_pred   <- add_preds_to_skeleton(sk_O3,   "O3",   models_final, horizon = 48)
sk_PM25_pred <- add_preds_to_skeleton(sk_PM25, "PM2.5",models_final, horizon = 48)

# Checks
tail(sk_NO2_pred, 10)
sk_NO2_pred %>% filter(Echeance_H %in% c(1,3,48)) %>% head(12)


library(dplyr)

# Seuils (µg/m3)
SEUILS_ALERTE <- c(
  "O3" = 120,
  "NO2" = 200,
  "PM2.5" = 25
)

add_statut <- function(df_pred) {
  stopifnot(all(c("Polluant","Pred_Mean","Pred_Max_80","Pred_Max_95") %in% names(df_pred)))
  
  df_pred %>%
    mutate(
      SEUIL_ALERTE = SEUILS_ALERTE[Polluant],
      Statut = case_when(
        Pred_Mean   > SEUIL_ALERTE ~ "ALERTE HAUTE",
        Pred_Max_80 > SEUIL_ALERTE ~ "ALERTE MOYENNE",
        Pred_Max_95 > SEUIL_ALERTE ~ "ALERTE POSSIBLE",
        TRUE ~ "Normal"
      )
    ) %>%
    select(-SEUIL_ALERTE)  # on enlève la colonne seuil si tu ne la veux pas dans le CSV
}

# ----------------------------
# Application à tes 3 dataframes
# ----------------------------
sk_NO2_pred  <- add_statut(sk_NO2_pred)
sk_O3_pred   <- add_statut(sk_O3_pred)
sk_PM25_pred <- add_statut(sk_PM25_pred)

# Checks rapides
table(sk_O3_pred$Statut)
sk_O3_pred %>% select(Station, Polluant, Echeance_H, Pred_Mean, Pred_Max_80, Pred_Max_95, Statut) %>% head(20)

# Sauvegarde
write.csv(sk_NO2_pred,  "data_shiny_no2_rural.csv",  row.names = FALSE)
write.csv(sk_O3_pred,   "data_shiny_o3_rural.csv",   row.names = FALSE)
write.csv(sk_PM25_pred, "data_shiny_pm25_rural.csv", row.names = FALSE)

