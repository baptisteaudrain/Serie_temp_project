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

# 1. CHARGEMENT & PRÉPARATION
# ---------------------------------------
df <- read.csv("mesure_horaire_view.csv")

# PARAMÉTRAGE CIBLE : OZONE PÉRIURBAIN
target_polluant <- "O3"
target_typologies <- c("Périurbaine") # On cible spécifiquement le périurbain

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

# GRAPHIQUE B : ACF/PACF BRUT
# Attends-toi à des vagues immenses sur l'ACF (Cycle jour/nuit)
ggtsdisplay(ts_train, lag.max=60, main="GRAPHIQUE B : Série Brute O3 (ACF/PACF)")




# ==============================================================================
# ETAPE 3-Bis : CHOIX DE LA DIFFÉRENCIATION (OZONE)
# ==============================================================================
library(forecast)
library(ggplot2)12

print("--- COMPARAISON DES DIFFÉRENCIATIONS (O3) ---")

# SCÉNARIO 1 : Juste le nettoyage saisonnier (24h)
# Pour l'Ozone, c'est souvent suffisant car il n'y a pas d'inertie "lourde" comme les particules
ts_diff_saison <- diff(ts_train, lag=24)
ggtsdisplay(ts_diff_saison, lag.max=60, 
            main="SCENARIO 1 : Différenciation Saisonnière seule (D=1)")

# SCÉNARIO 2 : La Totale (24h + 1h)
# Si le scénario 1 laisse des vagues, on teste celui-ci
ts_double_diff <- diff(ts_diff_saison, lag=1)
ggtsdisplay(ts_double_diff, lag.max=60, 
            main="SCENARIO 2 : Double Différenciation (D=1 + d=1)")




# ==============================================================================
# LE MATCH À TROIS : OZONE (Double Différenciation d=1, D=1)
# ==============================================================================
library(forecast)
library(Metrics)

print("--- SÉLECTION DU MODÈLE O3 (Sur Validation) ---")

# On fixe la partie saisonnière à (0,1,1) car tu as vu le pic négatif à 24
# On fixe d=1 car tu as choisi le scénario 2

# 1. CANDIDAT AR(2) : Suggéré par ta PACF (Pics à 1 et 2)
print("Entraînement AR(2)...")
model_ar2 <- Arima(ts_train, order=c(2,1,0), seasonal=list(order=c(0,1,1), period=24))

# 2. CANDIDAT MA(1) : Suggéré par ton ACF (Pic à 1)
print("Entraînement MA(1)...")
model_ma1 <- Arima(ts_train, order=c(0,1,1), seasonal=list(order=c(0,1,1), period=24))

# 3. CANDIDAT MIXTE : AR(2) + MA(1) - La Totale
print("Entraînement Mixte (2,1,1)...")
model_mix <- Arima(ts_train, order=c(2,1,1), seasonal=list(order=c(0,1,1), period=24))

# ------------------------------------------------------------------------------
# A. Comparaison AIC (Théorique)
# ------------------------------------------------------------------------------
print("--- RÉSULTATS AIC (Plus bas = Mieux) ---")
print(paste("AIC AR(2)      :", round(model_ar2$aic, 2)))
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

rmse_ar2  <- get_rmse_val(model_ar2)
rmse_ma1  <- get_rmse_val(model_ma1)
rmse_mix  <- get_rmse_val(model_mix)

print(paste("RMSE Val - AR(2)   :", round(rmse_ar2, 2)))
print(paste("RMSE Val - MA(1)   :", round(rmse_ma1, 2)))
print(paste("RMSE Val - Mixte   :", round(rmse_mix, 2)))

# ------------------------------------------------------------------------------
# C. SÉLECTION AUTOMATIQUE
# ------------------------------------------------------------------------------
scores <- c(AR2=rmse_ar2, MA1=rmse_ma1, Mixte=rmse_mix)
winner_name <- names(which.min(scores))

print(paste(">>> LE VAINQUEUR O3 EST :", winner_name))

if(winner_name == "AR2") {
  best_model <- model_ar2
  titre_model <- "SARIMA(2,1,0)(0,1,1)[24]"
} else if(winner_name == "MA1") {
  best_model <- model_ma1
  titre_model <- "SARIMA(0,1,1)(0,1,1)[24]"
} else {
  best_model <- model_mix
  titre_model <- "SARIMA(2,1,1)(0,1,1)[24]"
}

# Diagnostic résidus
checkresiduals(best_model)




# ==============================================================================
# FINAL O3 : VALIDATION ROBUSTE (SARIMA(2,1,1) + GARCH + ROLLING 48H)
# ==============================================================================
library(forecast)
library(rugarch)
library(ggplot2)
library(Metrics)

print("--- ETAPE FINALE O3 : Modélisation Hybride & Validation ---")

# 1. DÉFINITION DU VAINQUEUR (Mixte)
# ----------------------------------
# Issu de ton test précédent : ARIMA(2,1,1)(0,1,1)[24]
best_order <- c(2, 1, 1)
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
  # Incertitude
  geom_ribbon(aes(ymin=Bas95, ymax=Haut95, fill="Intervalle 95%"), alpha=0.2) +
  geom_ribbon(aes(ymin=Bas80, ymax=Haut80, fill="Intervalle 80%"), alpha=0.4) +
  
  # Recalage
  geom_vline(xintercept = lignes_recalage, linetype="dotted", color="black", alpha=0.5) +
  
  # Courbes
  geom_line(aes(y=Reel, color="Réel (O3)"), size=0.8) +
  geom_line(aes(y=Pred, color="Prévision 48h"), linetype="dashed", size=0.8) +
  
  scale_fill_manual(name = "Incertitude", values=c("Intervalle 95%"="orange", "Intervalle 80%"="#d35400")) +
  scale_color_manual(name = "Courbes", values=c("Réel (O3)"="#2980b9", "Prévision 48h"="#c0392b")) +
  
  labs(title = "Validation 48h Ozone (Périurbain)",
       subtitle = paste("RMSE Global :", round(rmse_final, 2), "µg/m³ - Modèle Hybride SARIMA(2,1,1) + GARCH"),
       y = "Concentration O3 (µg/m³)",
       caption = "Recalage sur données réelles toutes les 48h") +
  theme_minimal() +
  theme(legend.position = "bottom")