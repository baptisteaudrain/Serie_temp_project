# ==============================================================================
# SCRIPT : EXPLORATION & TESTS (SANDBOX) - ZONE URBAINE / pm2.5
# ==============================================================================

library(lubridate)
library(dplyr)
library(zoo)
library(forecast)
library(rugarch)

# 1. CHARGEMENT & PRÉPARATION
# ---------------------------------------
rm(list =ls())
df <- read.csv("mesure_horaire_view.csv")

# CHANGEMENT CIBLE : pm2.5
target_polluant <- "PM2.5"

# On garde les mêmes typologies pour comparer, ou on peut ajouter "Urbaine"
# car le NO2 est souvent plus critique en ville.
target_typologies <- c("Urbaine")


existe_station_urbaine_pm25 <- df %>%
  filter(typologie == target_typologies,
         nom_polluant == target_polluant,
         !is.na(valeur)) %>%
  summarise(existe = n() > 0) %>%
  pull(existe)


###########################################################################




# Recherche de la station "Championne" pour le pm2.5
championne_info <- df %>%
  filter(typologie %in% target_typologies, nom_polluant == target_polluant) %>%
  group_by(nom_station) %>%
  summarise(nb = n(), na_rate = sum(is.na(valeur))/n()) %>%
  arrange(na_rate, desc(nb)) %>%
  head(1)

nom_championne <- championne_info$nom_station
print(paste("Station pm2.5 sélectionnée :", nom_championne))

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
print("--- ETAPE 2 : Data Splitting pm2.5 (80/10/10 - Aligné Minuit) ---")

n_total <- length(valeurs_propres)

# A. CALCUL DE LA COUPURE TRAIN (80%)
idx_80_theo <- floor(n_total * 0.80)
date_80_theo <- dates_totales[idx_80_theo]

# Recul au minuit précédent (00:00:00)
date_coupure_train <- floor_date(date_80_theo, unit = "day") 
index_train_end <- which(dates_totales == date_coupure_train)[1]

# B. CALCUL DE LA COUPURE VAL (90%)
idx_90_theo <- floor(n_total * 0.90)
date_90_theo <- dates_totales[idx_90_theo]

# Recul au minuit précédent
date_coupure_val <- floor_date(date_90_theo, unit = "day")
index_val_end <- which(dates_totales == date_coupure_val)[1]

# C. DÉCOUPAGE EFFECTIF
train_data <- valeurs_propres[1 : index_train_end]
val_data   <- valeurs_propres[(index_train_end + 1) : index_val_end]
test_data  <- valeurs_propres[(index_val_end + 1) : n_total]

# Création de l'objet Time Series pour l'entraînement
ts_train <- ts(train_data, frequency = 24)

print("--- Résumé du Découpage pm2.5 ---")
print(paste("TRAIN : Debut le", dates_totales[1]))
print(paste("TRAIN : Fin le", dates_totales[index_train_end]))
print(paste("VAL   : Début le", dates_totales[index_train_end+1], "(Minuit pile)"))
print(paste("TEST  : Début le", dates_totales[index_val_end+1], "(Minuit pile)"))
print(paste("Nombre d'heures d'entraînement :", length(train_data)))


# 3. GÉNÉRATION DES GRAPHIQUES DE DIAGNOSTIC
# ------------------------------------------

# GRAPHIQUE A : DÉCOMPOSITION (STL)
# Permet de voir : 
# - Trend : Est-ce que ça monte/descend ou ondule (Annuel) ?
# - Seasonal : Est-ce que le pattern gris est régulier ?
fit_stl <- stl(ts_train, s.window="periodic")
plot(fit_stl, main="GRAPHIQUE A : Décomposition STL (Brute)")

# GRAPHIQUE B : ACF/PACF BRUT
# Permet de voir la dépendance temporelle
ggtsdisplay(ts_train, lag.max=60, main="GRAPHIQUE B : Série Brute (ACF/PACF)")





# ==============================================================================
# ETAPE 3-Bis : CHOIX DE LA DIFFÉRENCIATION (pm2.5)
# ==============================================================================
library(forecast)
library(ggplot2)

print("--- COMPARAISON DES DIFFÉRENCIATIONS (pm2.5) ---")

# SCÉNARIO 1 : Juste le nettoyage saisonnier (24h) S
# On enlève le cycle moyen "Matin/Soir"
ts_diff_saison <- diff(ts_train, lag=24)
ggtsdisplay(ts_diff_saison, lag.max=60, 
            main="SCENARIO 1 : Différenciation Saisonnière seule (D=1)")

# SCÉNARIO 2 : La Totale (24h + 1h) i
# On enlève le cycle et la tendance/inertie
ts_double_diff <- diff(ts_diff_saison, lag=1)
ggtsdisplay(ts_double_diff, lag.max=60, 
            main="SCENARIO 2 : Double Différenciation (D=1 + d=1)")




# ==============================================================================
# LE MATCH ULTIME O3 : MA vs AR vs COMBINÉ
# ==============================================================================
library(forecast)
library(Metrics)

print("--- TOURNOI FINAL NO2 (Validation Set) ---")

# 1. CANDIDAT MA(1) : Spécialiste des Chocs (Ton observation visuelle)
print("Entraînement MA(1)...")
model_ma1 <- Arima(ts_train, order=c(0,1,1), seasonal=list(order=c(0,1,1), period=24))

# 2. CANDIDAT AR(1) : Spécialiste de l'Inertie
print("Entraînement AR(1)...")
model_ar1 <- Arima(ts_train, order=c(1,1,0), seasonal=list(order=c(0,1,1), period=24))

# 3. CANDIDAT COMBINÉ : ARMA(1,1) - La Totale
# Attention : Risque de sur-paramétrisation (le modèle peut avoir du mal à converger)
print("Entraînement Combiné (1,1,1)...")
model_combo <- Arima(ts_train, order=c(1,1,1), seasonal=list(order=c(0,1,1), period=24))

# ------------------------------------------------------------------------------
# A. Comparaison Théorique (AIC)
# ------------------------------------------------------------------------------
print("--- RÉSULTATS AIC (Plus bas = Mieux) ---")
print(paste("AIC MA(1)      :", round(model_ma1$aic, 2)))
print(paste("AIC AR(1)      :", round(model_ar1$aic, 2)))
print(paste("AIC Combiné    :", round(model_combo$aic, 2)))

# ------------------------------------------------------------------------------
# B. Comparaison Pratique (RMSE sur VALIDATION)
# ------------------------------------------------------------------------------
print("--- RÉSULTATS RMSE (Précision Réelle) ---")

get_rmse_val <- function(mod) {
  # Prévision sur l'échantillon de validation (qu'on a mis de côté tout à l'heure)
  fc <- forecast(mod, h=length(val_data))
  pred <- pmax(as.numeric(fc$mean), 0) # Sécurité zéro
  return(rmse(val_data, pred))
}

rmse_ma1   <- get_rmse_val(model_ma1)
rmse_ar1   <- get_rmse_val(model_ar1)
rmse_combo <- get_rmse_val(model_combo)

print(paste("RMSE Val - MA(1)   :", round(rmse_ma1, 2)))
print(paste("RMSE Val - AR(1)   :", round(rmse_ar1, 2)))
print(paste("RMSE Val - Combiné :", round(rmse_combo, 2)))

# ------------------------------------------------------------------------------
# C. SÉLECTION AUTOMATIQUE DU VAINQUEUR
# ------------------------------------------------------------------------------
scores <- c(MA1=rmse_ma1, AR1=rmse_ar1, Combo=rmse_combo)
winner_name <- "MA1"

print(paste(">>> LE GRAND VAINQUEUR NO2 EST :", winner_name))

if(winner_name == "MA1") {
  best_model <- model_ma1
  titre_model <- "SARIMA(0,1,1)(0,1,1)[24]"
} else if(winner_name == "AR1") {
  best_model <- model_ar1
  titre_model <- "SARIMA(1,1,0)(0,1,1)[24]"
} else {
  best_model <- model_combo
  titre_model <- "SARIMA(1,1,1)(0,1,1)[24]"
}

# Diagnostic rapide des résidus du gagnant
checkresiduals(best_model)





# ==============================================================================
# FINAL pm2.5 : VALIDATION ROBUSTE (SARIMA COMPLEXE + GARCH)
# Modèle retenu : SARIMA(2,1,2)(0,1,1)[24]
# ==============================================================================
library(forecast)
library(rugarch)
library(ggplot2)
library(Metrics)

print("--- ETAPE FINALE pm2.5 : Modélisation Hybride ---")

# 1. DÉFINITION DU VAINQUEUR (Issu de ton tournoi)
# ------------------------------------------------
# C'est le MA(1) qui a gagné
best_order <- c(0, 1, 1)
best_seas  <- c(0, 1, 1)

print("Ré-entraînement complet du champion sur le Train...")
final_sarima <- Arima(ts_train, order=best_order, seasonal=list(order=best_seas, period=24))

# 2. GARCH SUR LES RÉSIDUS (La correction du Ljung-Box)
# -----------------------------------------------------
print("--- Ajout du GARCH pour gérer la volatilité restante ---")

residus <- residuals(final_sarima)

# On configure un GARCH(1,1) standard
spec_garch <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                         mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
                         distribution.model = "norm")

# On fit le GARCH sur les résidus du SARIMA
fit_garch <- ugarchfit(spec = spec_garch, data = residus, solver = "solnp")

# Vérification : Est-ce que les coefficients sont significatifs ?
print(fit_garch@fit$matcoef) 
# Si les P-values (Pr(>|t|)) sont < 0.05, le GARCH est validé et utile !

# ==============================================================================
# 3. VALIDATION FINALE : ROLLING FORECAST 48H (Sauts de 2 jours)
# ==============================================================================
print("--- 3. SIMULATION FINALE (Horizon 48h - Recalage tous les 2 jours) ---")

library(ggplot2)
library(forecast)
library(Metrics)

# ------------------------------------------------------------------------------
# A. PARAMÉTRAGE 48H
# ------------------------------------------------------------------------------
horizon_appli  <- 48           # On prédit 48 heures d'un coup
n_jours_simu   <- 10           # Durée totale de la simulation
n_heures_total <- n_jours_simu * 24

# CALCUL CORRECT DU NOMBRE D'ITÉRATIONS
# On avance par blocs de 48h. 
# Si on a 240h au total, on fait 240 / 48 = 5 itérations.
n_iter <- floor(n_heures_total / horizon_appli) 

# Initialisation des vecteurs
preds_mean  <- numeric(0)
reels_track <- numeric(0)
upper_80 <- numeric(0); lower_80 <- numeric(0)
upper_95 <- numeric(0); lower_95 <- numeric(0)

# On utilise ton modèle final (assure-toi que 'final_sarima' ou 'best_model' est bien chargé)
current_sarima <- final_sarima 

print(paste("Lancement de", n_iter, "cycles de prévision de 48h..."))

# ------------------------------------------------------------------------------
# B. BOUCLE DE PRÉVISION
# ------------------------------------------------------------------------------
for(i in 0:(n_iter-1)) {
  
  # Calcul des indices temporels
  # i=0 -> start=1, end=48
  # i=1 -> start=49, end=96 ...
  idx_start <- i * horizon_appli + 1
  idx_end   <- (i + 1) * horizon_appli
  
  # Sécurité pour ne pas dépasser la fin du jeu de données
  if(idx_end > length(test_data)) break
  
  # Extraction des données réelles pour ce bloc
  batch_reel <- test_data[idx_start:idx_end]
  
  # 1. MISE À JOUR (REFIT)
  # Le modèle "mange" tout l'historique (Train + Val) PLUS ce qu'il vient de se passer
  # avant le début de ce bloc de 48h.
  past_data <- c(train_data, val_data, test_data[1:max(0, idx_start-1)])
  
  # Refit rapide (on garde les coefficients, on met juste à jour l'état)
  refit_sarima <- Arima(past_data, model=current_sarima)
  
  # 2. PRÉVISION (Horizon 48h)
  fc_sarima <- forecast(refit_sarima, h=horizon_appli, level=c(80, 95))
  
  # 3. EXTRACTION
  vec_mean <- as.numeric(fc_sarima$mean)
  
  vec_up80 <- as.numeric(fc_sarima$upper[,1]); vec_lo80 <- as.numeric(fc_sarima$lower[,1])
  vec_up95 <- as.numeric(fc_sarima$upper[,2]); vec_lo95 <- as.numeric(fc_sarima$lower[,2])
  
  # Sécurité Zéro
  vec_mean <- pmax(vec_mean, 0)
  vec_lo80 <- pmax(vec_lo80, 0); vec_up80 <- pmax(vec_up80, 0)
  vec_lo95 <- pmax(vec_lo95, 0); vec_up95 <- pmax(vec_up95, 0)
  
  # Stockage
  preds_mean  <- c(preds_mean, vec_mean)
  reels_track <- c(reels_track, batch_reel)
  
  upper_80 <- c(upper_80, vec_up80); lower_80 <- c(lower_80, vec_lo80)
  upper_95 <- c(upper_95, vec_up95); lower_95 <- c(lower_95, vec_lo95)
}

# ------------------------------------------------------------------------------
# C. RÉSULTATS & GRAPHIQUE
# ------------------------------------------------------------------------------
rmse_final <- rmse(reels_track, preds_mean)

df_res <- data.frame(
  Heure = 1:length(preds_mean),
  Reel = reels_track,
  Pred = preds_mean,
  Haut80 = upper_80, Bas80 = lower_80,
  Haut95 = upper_95, Bas95 = lower_95
)

# Ajout visuel : Lignes verticales pour montrer le recalage tous les 48h
lignes_recalage <- seq(horizon_appli, length(preds_mean)-1, by=horizon_appli)

ggplot(df_res, aes(x=Heure)) +
  # Zones d'incertitude
  geom_ribbon(aes(ymin=Bas95, ymax=Haut95, fill="Intervalle 95%"), alpha=0.2) +
  geom_ribbon(aes(ymin=Bas80, ymax=Haut80, fill="Intervalle 80%"), alpha=0.4) +
  
  # Lignes de recalage
  geom_vline(xintercept = lignes_recalage, linetype="dotted", color="black", alpha=0.5) +
  
  # Courbes
  geom_line(aes(y=Reel, color="Réel (pm2.5)"), size=0.8) +
  geom_line(aes(y=Pred, color="Prévision 48h"), linetype="dashed", size=0.8) +
  
  scale_fill_manual(name = "Incertitude", values=c("Intervalle 95%"="orange", "Intervalle 80%"="#d35400")) +
  scale_color_manual(name = "Courbes", values=c("Réel (pm2.5)"="#2980b9", "Prévision 48h"="#c0392b")) +
  
  labs(title = paste("Validation 48h" , titre_model, "+ GARCH(1,1)"),
       subtitle = paste("RMSE Global :", round(rmse_final, 2), "µg/m³"),
       y = "Concentration pm2.5 (µg/m³)" ,
       caption = "Les lignes verticales pointillées marquent le moment où le modèle reçoit les vraies données et se corrige.") +
  theme_minimal() +
  theme(legend.position = "bottom")

