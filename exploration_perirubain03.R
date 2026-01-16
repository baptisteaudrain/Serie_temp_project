# ==============================================================================
# SCRIPT DE MODÉLISATION ROBUSTE : OZONE (O3) - PÉRIURBAIN
# ==============================================================================
rm(list = ls()) # Nettoyage mémoire

library(dplyr)
library(lubridate)
library(zoo)
library(forecast)
library(ggplot2)
library(Metrics)
library(rugarch)
library(tidyr)

# 1. CHARGEMENT & PRÉPARATION
# ---------------------------------------
df <- read.csv("mesure_horaire_view.csv")

# PARAMÉTRAGE CIBLE : OZONE PÉRIURBAIN
target_polluant <- "O3"
target_typologies <-  c("Périurbaine", "Rurale proche Zone Urbaine") # On cible spécifiquement le périurbain

# Recherche de la station "Championne"
championne_info <- df %>%
  filter(typologie %in% target_typologies, nom_polluant == target_polluant) %>%
  group_by(nom_station) %>%
  summarise(nb = n(), na_rate = sum(is.na(valeur))/n()) %>%
  arrange(na_rate, desc(nb)) %>%
  head(1)

nom_championne <- championne_info$nom_station
print(paste("Station O3 sélectionnée :", nom_championne))

# Filtrage et Nettoyage
df_champ <- df %>%
  filter(nom_station == nom_championne, nom_polluant == target_polluant) %>%
  mutate(date_fin = ymd_hms(date_fin, quiet=TRUE)) %>%
  arrange(date_fin)

# Imputation sur grille horaire complète
grille <- data.frame(date_fin = seq(min(df_champ$date_fin), max(df_champ$date_fin), by="hour"))
df_clean <- merge(grille, df_champ, by="date_fin", all.x=TRUE)
valeurs_propres <- na.approx(df_clean$valeur, rule=2) # Interpolation linéaire
dates_totales <- df_clean$date_fin

# ==============================================================================
# 2. DATA SPLITTING INTELLIGENT (ALIGNÉ SUR MINUIT)
# ==============================================================================
print("--- ETAPE 2 : Data Splitting O3 (80/10/10 - Aligné Minuit) ---")

n_total <- length(valeurs_propres)

# A. CALCUL DE LA COUPURE TRAIN (80%)
idx_80_theo <- floor(n_total * 0.80)
date_80_theo <- dates_totales[idx_80_theo]
date_coupure_train <- floor_date(date_80_theo, unit = "day") 
index_train_end <- which(dates_totales == date_coupure_train)[1]

# B. CALCUL DE LA COUPURE VAL (90%)
idx_90_theo <- floor(n_total * 0.90)
date_90_theo <- dates_totales[idx_90_theo]
date_coupure_val <- floor_date(date_90_theo, unit = "day")
index_val_end <- which(dates_totales == date_coupure_val)[1]

# C. DÉCOUPAGE
train_data <- valeurs_propres[1 : index_train_end]
val_data   <- valeurs_propres[(index_train_end + 1) : index_val_end]
test_data  <- valeurs_propres[(index_val_end + 1) : n_total]

# Objet Time Series
ts_train <- ts(train_data, frequency = 24)

print("--- Résumé du Découpage O3 ---")
print(paste("TRAIN : Fin le", dates_totales[index_train_end]))
print(paste("VAL   : Début le", dates_totales[index_train_end+1]))
print(paste("TEST  : Début le", dates_totales[index_val_end+1]))

# ==============================================================================
# 3. DIAGNOSTIC VISUEL (STL & ACF BRUT)
# ==============================================================================
print("--- ETAPE 3 : Diagnostic Visuel ---")

# GRAPHIQUE A : DÉCOMPOSITION (STL)
# Attends-toi à une saisonnalité (barre grise 'seasonal') très dense et régulière
fit_stl <- stl(ts_train, s.window="periodic")
plot(fit_stl, main="GRAPHIQUE A : Décomposition STL (O3 Brut)")


stl_df <- as.data.frame(fit_stl$time.series)
# Renaming standard STL components to English
colnames(stl_df) <- c("Seasonality", "Trend", "Remainder")

# Adding Raw Data and Date
stl_df$Raw_Data <- as.numeric(ts_train)
stl_df$Date <- dates_totales[1:nrow(stl_df)]

# 2. Zoom on a representative window (e.g., 15 days)
# --------------------------------------------------
periode_zoom <- stl_df %>%
  filter(Date >= min(Date) & Date <= min(Date) + days(15))

# 3. Reshaping for ggplot (Long Format)
# -------------------------------------
df_plot <- periode_zoom %>%
  pivot_longer(cols = c("Raw_Data", "Seasonality", "Trend", "Remainder"),
               names_to = "Component", values_to = "Value") %>%
  # Force the order of facets: Raw Data first, then components
  mutate(Component = factor(Component, levels = c("Raw_Data", "Seasonality", "Trend", "Remainder")))

# 4. Plotting (English Labels)
# ----------------------------
ggplot(df_plot, aes(x = Date, y = Value, color = Component)) +
  geom_line(size = 0.8) +
  facet_grid(Component ~ ., scales = "free_y") + 
  
  theme_minimal() +
  
  # --- NOUVELLE PALETTE : DÉGRADÉ DE BLEUS ---
  # On utilise des codes hexadécimaux pour un contrôle précis du dégradé,
  # du plus foncé (Raw Data) au plus clair (Remainder).
  scale_color_manual(values = c(
    "Raw_Data"    = "#08306b", # Bleu nuit très foncé (Données brutes)
    "Trend"       = "#2171b5", # Bleu foncé (Tendance)
    "Seasonality" = "#4292c6", # Bleu moyen vif (Saisonnalité - bien visible)
    "Remainder"   = "#9ecae1"  # Bleu clair grisé (Bruit de fond)
  )) +
  # -------------------------------------------

labs(
  title = "STL Decomposition: 15-Day Zoom (Ozone)",
  subtitle = "Visual evidence of strong 24h Seasonality justifying seasonal differentiation",
  x = "Date",
  y = "Concentration (µg/m³)"
) +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 12, face = "bold", color = "#08306b"), # Titres des facettes en bleu foncé
    plot.title = element_text(size = 14, face = "bold", color = "#08306b"),
    plot.subtitle = element_text(size = 11, color = "#2171b5")
  )




# GRAPHIQUE B : ACF/PACF BRUT
# Attends-toi à des vagues immenses sur l'ACF (Cycle jour/nuit)
ggtsdisplay(ts_train, lag.max=60)




# ==============================================================================
# ETAPE 3-Bis : CHOIX DE LA DIFFÉRENCIATION (OZONE)
# ==============================================================================
library(forecast)
library(ggplot2)

print("--- COMPARAISON DES DIFFÉRENCIATIONS (O3) ---")

# SCÉNARIO 1 : Juste le nettoyage saisonnier (24h)
# Pour l'Ozone, c'est souvent suffisant car il n'y a pas d'inertie "lourde" comme les particules
ts_diff_saison <- diff(ts_train, lag=24)
ggtsdisplay(ts_diff_saison, lag.max=60)

# SCÉNARIO 2 : La Totale (24h + 1h)
# Si le scénario 1 laisse des vagues, on teste celui-ci
ts_double_diff <- diff(ts_diff_saison, lag=1)
ggtsdisplay(ts_double_diff, lag.max=60)




# ==============================================================================
# LE MATCH À TROIS : OZONE (Double Différenciation d=1, D=1)
# ==============================================================================
library(forecast)
library(Metrics)

print("--- SÉLECTION DU MODÈLE O3 (Sur Validation) ---")

# On fixe la partie saisonnière à (0,1,1) car tu as vu le pic négatif à 24
# On fixe d=1 car tu as choisi le scénario 2

print("Entraînement AR(1)...")
model_ar1 <- Arima(ts_train, order=c(1,1,0), seasonal=list(order=c(0,1,1), period=24))

print("Entraînement MA(1)...")
model_ma1 <- Arima(ts_train, order=c(0,1,1), seasonal=list(order=c(0,1,1), period=24))

print("Entraînement Mixte (1,1,1)...")
model_mix <- Arima(ts_train, order=c(1,1,1), seasonal=list(order=c(0,1,1), period=24))

# ------------------------------------------------------------------------------
# A. Comparaison AIC (Théorique)
# ------------------------------------------------------------------------------
print("--- RÉSULTATS AIC (Plus bas = Mieux) ---")
print(paste("AIC AR(1)      :", round(model_ar1$aic, 2)))
print(paste("AIC MA(1)      :", round(model_ma1$aic, 2)))
print(paste("AIC Mixte      :", round(model_mix$aic, 2)))

# ------------------------------------------------------------------------------
# B. Comparaison Pratique (RMSE sur VALIDATION)
# ------------------------------------------------------------------------------
print("--- RÉSULTATS RMSE (Précision Réelle) ---")

get_rmse_val <- function(mod) {
  # Prévision sur validation
  fc <- forecast(mod, h=length(val_data))
  pred <- pmax(as.numeric(fc$mean), 0)
  return(rmse(val_data, pred))
}

rmse_ar1  <- get_rmse_val(model_ar1)
rmse_ma1  <- get_rmse_val(model_ma1)
rmse_mix  <- get_rmse_val(model_mix)

print(paste("RMSE Val - AR(1)   :", round(rmse_ar1, 2)))
print(paste("RMSE Val - MA(1)   :", round(rmse_ma1, 2)))
print(paste("RMSE Val - Mixte   :", round(rmse_mix, 2)))

# ------------------------------------------------------------------------------
# SÉLECTION INTELLIGENTE (Arbitrage RMSE vs AIC)
# ------------------------------------------------------------------------------

# 1. On rassemble les scores dans un tableau propre
df_scores <- data.frame(
  Modele = c("MA1", "AR1", "Combo"),
  RMSE   = c(rmse_ma1, rmse_ar1, rmse_mix),
  AIC    = c(model_ma1$aic, model_ar1$aic, model_mix$aic)
)

print("--- TABLEAU DES SCORES ---")
print(df_scores)

# 2. FONCTION DE DÉCISION
# tolerance_pct : Si un modèle est à moins de X% du meilleur RMSE, 
# on considère qu'il est "aussi bon" en prévision.
select_smart_winner <- function(df, tolerance_pct = 0.02) { # 0.02 = 2% de tolérance
  
  # A. Trouver le meilleur RMSE absolu
  best_rmse <- min(df$RMSE)
  
  # B. Seuil de tolérance (Best RMSE + 2%)
  threshold <- best_rmse * (1 + tolerance_pct)
  
  # C. On garde les "Finalistes" (ceux qui sont sous le seuil)
  candidates <- df[df$RMSE <= threshold, ]
  
  print(paste("--> Meilleur RMSE :", round(best_rmse, 4)))
  print(paste("--> Seuil tolérance (2%) :", round(threshold, 4)))
  print(paste("--> Modèles finalistes retenus pour l'AIC :", paste(candidates$Modele, collapse=", ")))
  
  # D. Parmi les finalistes, le vainqueur est celui avec le MINIMUM d'AIC
  winner_row <- candidates[which.min(candidates$AIC), ]
  
  return(winner_row$Modele)
}

# 3. ÉLECTION
winner_name <- select_smart_winner(df_scores, tolerance_pct = 0.02)

print("==================================================")
print(paste(">>> VAINQUEUR OFFICIEL :", winner_name))
print("==================================================")
if(winner_name == "AR1") {
  best_model <- model_ar1
  titre_model <- "SARIMA(1,1,0)(0,1,1)[24]"
} else if(winner_name == "MA1") {
  best_model <- model_ma1
  titre_model <- "SARIMA(0,1,1)(0,1,1)[24]"
} else {
  best_model <- model_mix
  titre_model <- "SARIMA(1,1,1)(0,1,1)[24]"
}

# Diagnostic résidus
checkresiduals(best_model)




# ==============================================================================
# FINAL O3 : VALIDATION ROBUSTE 
# ==============================================================================
library(forecast)
library(rugarch)
library(ggplot2)
library(Metrics)

print("--- ETAPE FINALE O3 : Modélisation Hybride & Validation ---")

# 1. DÉFINITION DU VAINQUEUR (Mixte)
# ----------------------------------

best_order <- c(1, 1, 0)
best_seas  <- c(0, 1, 1)

print("Ré-entraînement du champion SARIMA sur le Train...")
final_sarima <- Arima(ts_train, order=best_order, seasonal=list(order=best_seas, period=24))

# 2. DIAGNOSTIC VOLATILITÉ (Justification du GARCH)
# -------------------------------------------------
residus <- residuals(final_sarima)

# Test d'effet ARCH sur les résidus au carré
# Si p-value < 0.05, cela prouve qu'il faut un GARCH
test_arch <- Box.test(residus^2, type="Ljung-Box", lag=24)
print(paste("P-value Test ARCH (Volatilité) :", test_arch$p.value))

if(test_arch$p.value < 0.05) {
  print(">>> Volatilité hétéroscédastique détectée. GARCH validé.")
}

# Configuration GARCH(1,1)
spec_garch <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                         mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
                         distribution.model = "norm")

# Entraînement GARCH sur les résidus
fit_garch <- ugarchfit(spec = spec_garch, data = residus, solver = "solnp")
print("GARCH ajusté.")



# Une fois le fit_garch généré :
print("--- Coefficients du GARCH(1,1) ---")
matrice_coefs <- fit_garch@fit$matcoef
print(matrice_coefs)

# Astuce : Regardez la colonne "Pr(>|t|)". 
# Si elle est < 0.05, le coefficient est significatif.
# omega = constante, alpha1 = effet du choc passé (ARCH), beta1 = persistance (GARCH)

# 3. SIMULATION FINALE (ROLLING FORECAST 48H)
# ===========================================
print("--- Lancement de la simulation 48h avec recalage ---")

horizon_appli  <- 48           # Sauts de 48h
n_jours_simu   <- 10           # Durée totale
n_heures_total <- n_jours_simu * 24
n_iter         <- floor(n_heures_total / horizon_appli) 

# Vecteurs de stockage
preds_mean  <- numeric(0)
reels_track <- numeric(0)
upper_80 <- numeric(0); lower_80 <- numeric(0)
upper_95 <- numeric(0); lower_95 <- numeric(0)

# Copie du modèle
current_sarima <- final_sarima

for(i in 0:(n_iter-1)) {
  # Indices temporels
  idx_start <- i * horizon_appli + 1
  idx_end   <- (i + 1) * horizon_appli
  
  if(idx_end > length(test_data)) break
  
  batch_reel <- test_data[idx_start:idx_end]
  
  # A. MISE À JOUR (Le modèle apprend le passé récent)
  past_data <- c(train_data, val_data, test_data[1:max(0, idx_start-1)])
  refit_sarima <- Arima(past_data, model=current_sarima)
  
  # B. PRÉVISION 48H
  fc_sarima <- forecast(refit_sarima, h=horizon_appli, level=c(80, 95))
  
  # C. STOCKAGE
  vec_mean <- as.numeric(fc_sarima$mean)
  
  vec_up80 <- as.numeric(fc_sarima$upper[,1]); vec_lo80 <- as.numeric(fc_sarima$lower[,1])
  vec_up95 <- as.numeric(fc_sarima$upper[,2]); vec_lo95 <- as.numeric(fc_sarima$lower[,2])
  
  # Sécurité Zéro
  vec_mean <- pmax(vec_mean, 0)
  vec_lo80 <- pmax(vec_lo80, 0); vec_up80 <- pmax(vec_up80, 0)
  vec_lo95 <- pmax(vec_lo95, 0); vec_up95 <- pmax(vec_up95, 0)
  
  preds_mean  <- c(preds_mean, vec_mean)
  reels_track <- c(reels_track, batch_reel)
  
  upper_80 <- c(upper_80, vec_up80); lower_80 <- c(lower_80, vec_lo80)
  upper_95 <- c(upper_95, vec_up95); lower_95 <- c(lower_95, vec_lo95)
}

# 4. GRAPHIQUE FINAL
# ==================
rmse_final <- rmse(reels_track, preds_mean)

df_res <- data.frame(
  Heure = 1:length(preds_mean),
  Reel = reels_track,
  Pred = preds_mean,
  Haut80 = upper_80, Bas80 = lower_80,
  Haut95 = upper_95, Bas95 = lower_95
)

# Lignes de recalage
lignes_recalage <- seq(horizon_appli, length(preds_mean)-1, by=horizon_appli)

ggplot(df_res, aes(x=Heure)) +
  # Incertitude (Uncertainty) - Traduction des labels dans 'fill'
  geom_ribbon(aes(ymin=Bas95, ymax=Haut95, fill="95% Confidence Interval"), alpha=0.2) +
  geom_ribbon(aes(ymin=Bas80, ymax=Haut80, fill="80% Confidence Interval"), alpha=0.4) +
  
  # Recalage (Refit lines)
  geom_vline(xintercept = lignes_recalage, linetype="dotted", color="black", alpha=0.5) +
  
  # Courbes (Curves) - Traduction des labels dans 'color'
  geom_line(aes(y=Reel, color="Actual (O3)"), size=0.8) +
  geom_line(aes(y=Pred, color="48h Forecast"), linetype="dashed", size=0.8) +
  
  # Définition manuelle des couleurs avec les noms anglais correspondants
  scale_fill_manual(name = "Uncertainty", 
                    values=c("95% Confidence Interval"="orange", 
                             "80% Confidence Interval"="#d35400")) +
  
  scale_color_manual(name = "Series", 
                     values=c("Actual (O3)"="#2980b9", 
                              "48h Forecast"="#c0392b")) +
  
  # Labels en anglais et suppression du titre
  labs(title = NULL, # Pas de titre
       subtitle = paste("Global RMSE:", round(rmse_final, 2), "µg/m³"),
       x = "Hour",
       y = "O3 Concentration (µg/m³)",
       caption = "Model refit on actual data every 48h") +
  
  theme_minimal() +
  theme(legend.position = "bottom")

