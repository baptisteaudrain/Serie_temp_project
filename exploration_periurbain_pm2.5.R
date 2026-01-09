# ==============================================================================
# SCRIPT DE MODÉLISATION ROBUSTE (DONNÉES BRUTES)
# ==============================================================================
rm(list = ls())

library(dplyr)
library(lubridate)
library(zoo)
library(forecast)
library(ggplot2)
library(Metrics)
library(rugarch)

# 1. CHARGEMENT & PRÉPARATION (Standard)
# ---------------------------------------
df <- read.csv("mesure_horaire_view.csv")
target_polluant <- "PM2.5"
target_typologies <- c("Périurbaine", "Rurale proche Zone Urbaine")

# Recherche championne
championne_info <- df %>%
  filter(typologie %in% target_typologies, nom_polluant == target_polluant) %>%
  group_by(nom_station) %>%
  summarise(nb = n(), na_rate = sum(is.na(valeur))/n()) %>%
  arrange(na_rate, desc(nb)) %>%
  head(1)

nom_championne <- championne_info$nom_station
print(paste("Station sélectionnée :", nom_championne))

df_champ <- df %>%
  filter(nom_station == nom_championne, nom_polluant == target_polluant) %>%
  mutate(date_fin = ymd_hms(date_fin, quiet=TRUE)) %>%
  arrange(date_fin)

# Imputation
grille <- data.frame(date_fin = seq(min(df_champ$date_fin), max(df_champ$date_fin), by="hour"))
df_clean <- merge(grille, df_champ, by="date_fin", all.x=TRUE)
valeurs_propres <- na.approx(df_clean$valeur, rule=2)
dates_totales <- df_clean$date_fin

# ==============================================================================
# 2. DATA SPLITTING INTELLIGENT (ALIGNÉ SUR MINUIT)
# ==============================================================================
print("--- ETAPE 2 : Data Splitting (80/10/10 - Aligné Minuit) ---")

n_total <- length(valeurs_propres)

# A. CALCUL DE LA COUPURE TRAIN (80%)
idx_80_theo <- floor(n_total * 0.80)
date_80_theo <- dates_totales[idx_80_theo]
# On recule au minuit précédent pour avoir des journées complètes
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

# Création objets TS pour train (nécessaire pour Arima)
ts_train <- ts(train_data, frequency = 24)

print("--- Résumé du Découpage ---")
print(paste("TRAIN : Fin le", dates_totales[index_train_end]))
print(paste("VAL   : Début le", dates_totales[index_train_end+1], "(Minuit pile)"))
print(paste("TEST  : Début le", dates_totales[index_val_end+1], "(Minuit pile)"))




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
# ETAPE 3-Bis : CHOIX DE LA DIFFÉRENCIATION (SCÉNARIOS)
# ==============================================================================
library(forecast)
library(ggplot2)

print("--- COMPARAISON DES DIFFÉRENCIATIONS ---")

# SCÉNARIO 1 : Juste le nettoyage saisonnier (24h)
# ------------------------------------------------
ts_diff_saison <- diff(ts_train, lag=24)
ggtsdisplay(ts_diff_saison, lag.max=60, 
            main="SCENARIO 1 : Différenciation Saisonnière seule (D=1)")

# SCÉNARIO 2 : Nettoyage Saisonnier (24h) + Nettoyage Tendance (1h)
# -----------------------------------------------------------------
ts_double_diff <- diff(ts_diff_saison, lag=1)
ggtsdisplay(ts_double_diff, lag.max=60, 
            main="SCENARIO 2 : Double Différenciation (D=1 + d=1)")



# ==============================================================================
# SCRIPT CORRIGÉ : SÉLECTION SUR VALIDATION (ET NON TEST)
# ==============================================================================

print("--- 1. LE MATCH À TROIS (Sélection sur VALIDATION SET) ---")

# A. Définition des 3 Candidats (Entraînés sur TRAIN)
# ---------------------------------------------------
# 1. Le MA(2)
model_ma2 <- Arima(ts_train, order=c(0,1,2), seasonal=list(order=c(0,1,1), period=24))

# 2. Le AR(2)
model_ar2 <- Arima(ts_train, order=c(2,1,0), seasonal=list(order=c(0,1,1), period=24))

# 3. Le Mixte(1,1)
model_mix <- Arima(ts_train, order=c(1,1,1), seasonal=list(order=c(0,1,1), period=24))

# B. Comparaison AIC (Théorique - calculé sur le Train)
print(paste("AIC MA(2)   :", round(model_ma2$aic, 2)))
print(paste("AIC AR(2)   :", round(model_ar2$aic, 2)))
print(paste("AIC Mixte   :", round(model_mix$aic, 2)))

# C. Comparaison RMSE PRATIQUE (Calculé sur VALIDATION)
# C'est ici la correction : on utilise val_data pour choisir le meilleur
print("--- Calcul des erreurs sur le jeu de VALIDATION ---")

get_rmse_val <- function(mod) {
  # On prédit sur la durée totale du set de validation
  fc <- forecast(mod, h=length(val_data))
  pred <- pmax(as.numeric(fc$mean), 0)
  return(rmse(val_data, pred))
}

rmse_ma_val  <- get_rmse_val(model_ma2)
rmse_ar_val  <- get_rmse_val(model_ar2)
rmse_mix_val <- get_rmse_val(model_mix)

print(paste("RMSE Val - MA(2)   :", round(rmse_ma_val, 2)))
print(paste("RMSE Val - AR(2)   :", round(rmse_ar_val, 2)))
print(paste("RMSE Val - Mixte   :", round(rmse_mix_val, 2)))

# SÉLECTION AUTOMATIQUE DU VAINQUEUR
scores <- c(MA2=rmse_ma_val, AR2=rmse_ar_val, Mixte=rmse_mix_val)
winner_name <- names(which.min(scores))
print(paste(">>> LE VAINQUEUR (élus sur Validation) EST :", winner_name))

# Assignation du meilleur modèle pour la suite
if(winner_name == "MA2") {
  best_model <- model_ma2
  titre_model <- "SARIMA(0,1,2)(0,1,1)[24]"
} else if(winner_name == "AR2") {
  best_model <- model_ar2
  titre_model <- "SARIMA(2,1,0)(0,1,1)[24]"
} else {
  best_model <- model_mix
  titre_model <- "SARIMA(1,1,1)(0,1,1)[24]"
}

# Diagnostic rapide du vainqueur
checkresiduals(best_model)




# ==============================================================================
# 2. AJOUT DU GARCH (Gestion de la Volatilité)
# ==============================================================================
print("--- 2. ANALYSE DES RÉSIDUS ET AJOUT GARCH ---")

# On récupère les résidus du vainqueur
residus_winner <- residuals(best_model)

# Test d'effet ARCH (Est-ce que la variance change dans le temps ?)
test_arch <- Box.test(residus_winner^2, type="Ljung-Box", lag=24)
print(paste("P-value Test ARCH :", test_arch$p.value))

fit_garch <- NULL
if(test_arch$p.value < 0.05) {
  print(">>> Volatilité hétéroscédastique détectée. On ajoute un GARCH(1,1).")
  
  # Configuration standard GARCH(1,1)
  spec_garch <- ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)),
                           mean.model=list(armaOrder=c(0,0), include.mean=FALSE))
  
  # Entraînement sur les résidus
  fit_garch <- ugarchfit(spec=spec_garch, data=residus_winner, solver="solnp")
  print(fit_garch) # Affiche les coefficients alpha/beta
} else {
  print(">>> Pas de volatilité significative. Le SARIMA seul suffit.")
}

#  
# (Sert à illustrer dans le rapport que GARCH encadre les périodes de calme et de tempête)

# ==============================================================================
# 3. VALIDATION FINALE : FAN CHART (80% + 95%)
# ==============================================================================
print("--- 3. SIMULATION FINALE (Fan Chart 80% + 95%) ---")

library(ggplot2)
library(forecast)
library(Metrics)

# Paramètres de simulation
horizon_appli  <- 48           # On prédit 48 heures d'un coup
n_jours_simu   <- 10           # Durée totale de la simulation
n_heures_total <- n_jours_simu * 24

# CALCUL CORRECT DU NOMBRE D'ITÉRATIONS
# On avance par blocs de 48h. 
# Si on a 240h au total, on fait 240 / 48 = 5 itérations.
n_iter <- floor(n_heures_total / horizon_appli) 

# Initialisation des vecteurs de stockage
preds_mean  <- numeric(0)
reels_track <- numeric(0)

# Stockage pour l'intervalle 80% (Zone Probable)
upper_80 <- numeric(0)
lower_80 <- numeric(0)

# Stockage pour l'intervalle 95% (Zone Sécurité)
upper_95 <- numeric(0)
lower_95 <- numeric(0)

# On part du meilleur modèle identifié
current_sarima <- best_model

print("Calcul en cours (Fan Chart)...")

for(i in 0:(n_iter-1)) {
  # Indices temporels pour ce jour
  idx_start <- i * horizon_appli + 1
  idx_end   <- (i + 1) * horizon_appli
  
  batch_reel <- test_data[idx_start:idx_end]
  
  # 1. Mise à jour du modèle (Refit)
  # On reconstruit l'historique connu jusqu'à la veille
  # (Ajoute 'val_data' dans le c(...) ci-dessous si tu avais un set de validation séparé)
  past_data <- c(train_data, test_data[1:max(0, idx_start-1)])
  
  refit_sarima <- Arima(past_data, model=current_sarima)
  
  # 2. Prévision avec DOUBLE NIVEAU (80% et 95%)
  fc_sarima <- forecast(refit_sarima, h=horizon_appli, level=c(80, 95))
  
  # 3. Extraction des données
  vec_mean <- as.numeric(fc_sarima$mean)
  
  # Attention : forecast() met les niveaux dans l'ordre demandé
  # Colonne 1 = 80%, Colonne 2 = 95%
  vec_up80 <- as.numeric(fc_sarima$upper[,1])
  vec_lo80 <- as.numeric(fc_sarima$lower[,1])
  
  vec_up95 <- as.numeric(fc_sarima$upper[,2])
  vec_lo95 <- as.numeric(fc_sarima$lower[,2])
  
  # 4. Sécurité Zéro (Pas de pollution négative)
  vec_mean <- pmax(vec_mean, 0)
  vec_lo80 <- pmax(vec_lo80, 0); vec_up80 <- pmax(vec_up80, 0)
  vec_lo95 <- pmax(vec_lo95, 0); vec_up95 <- pmax(vec_up95, 0)
  
  # 5. Stockage
  preds_mean  <- c(preds_mean, vec_mean)
  reels_track <- c(reels_track, batch_reel)
  
  upper_80 <- c(upper_80, vec_up80); lower_80 <- c(lower_80, vec_lo80)
  upper_95 <- c(upper_95, vec_up95); lower_95 <- c(lower_95, vec_lo95)
}

# Calcul du RMSE sur la moyenne
rmse_final <- rmse(reels_track, preds_mean)

# Création du DataFrame Final
df_res <- data.frame(
  Heure = 1:length(preds_mean),
  Reel = reels_track,
  Pred = preds_mean,
  Haut80 = upper_80, Bas80 = lower_80,
  Haut95 = upper_95, Bas95 = lower_95
)

# Graphique Final (Fan Chart)
ggplot(df_res, aes(x=Heure)) +
  # 1. Zone 95% (Large et Claire) - En premier pour être au fond
  geom_ribbon(aes(ymin=Bas95, ymax=Haut95, fill="Intervalle 95%"), alpha=0.2) +
  
  # 2. Zone 80% (Serrée et Foncée) - Par dessus
  geom_ribbon(aes(ymin=Bas80, ymax=Haut80, fill="Intervalle 80%"), alpha=0.4) +
  
  # 3. Lignes (Réalité et Prévision)
  geom_line(aes(y=Reel, color="Réel"), size=0.8) +
  geom_line(aes(y=Pred, color="Prévision"), linetype="dashed", size=0.8) +
  
  # 4. Couleurs et Légendes
  scale_fill_manual(name = "Incertitude", 
                    values=c("Intervalle 95%"="orange", "Intervalle 80%"="#d35400")) +
  
  scale_color_manual(name = "Courbes", 
                     values=c("Réel"="#2980b9", "Prévision"="#c0392b")) +
  
  labs(title = paste("Validation Finale :", titre_model, "+ GARCH"),
       subtitle = paste("RMSE Glissant (10 jours) :", round(rmse_final, 2), "µg/m³"),
       y = "Concentration PM2.5 (µg/m³)",
       caption = "Zone Foncée = 80% (Probable) | Zone Claire = 95% (Sécurité)") +
  
  theme_minimal() +
  theme(legend.position = "bottom")