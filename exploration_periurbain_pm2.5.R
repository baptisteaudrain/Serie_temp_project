# ==============================================================================
# SCRIPT 1 : EXPLORATION & TESTS (SANDBOX)
# ==============================================================================
# Ce script sert à analyser les données manuellement. 
# Il n'est PAS exécuté par l'application finale.

rm(list = ls()) # On vide l'environnement pour tester proprement

# 1. Chargement & Analyse Rapide
# ------------------------------
df <- read.csv("mesure_horaire_view.csv")

# ==============================================================================
# 2. RECHERCHE DE LA STATION CHAMPIONNE (Le bout de code manquant)
# ==============================================================================
# Objectif : Trouver la station Périurbaine / PM2.5 la plus propre (Moins de NA, plus d'historique)

# Définition de la cible
target_polluant <- "PM2.5"
target_typologies <- c("Périurbaine", "Rurale proche Zone Urbaine")

print("Classement des meilleures stations candidates...")

classement_stations <- df %>%
  # a. Filtrage de la cible
  filter(typologie %in% target_typologies,
         nom_polluant == target_polluant) %>%
  
  # b. Calcul des indicateurs de qualité par station
  group_by(nom_station, nom_com) %>%
  summarise(
    nb_mesures = n(),                        # Volume de données
    nb_na = sum(is.na(valeur)),              # Nombre de trous
    taux_na = round(sum(is.na(valeur)) / n() * 100, 2), # Pourcentage de vide
    date_debut = min(ymd_hms(date_fin), na.rm=TRUE),
    date_fin = max(ymd_hms(date_fin), na.rm=TRUE),
    .groups = "drop"
  ) %>%
  
  # c. Le Tri "Champion" : 
  # Critère 1 : Le moins de trous possible (taux_na croissant)
  # Critère 2 : Le plus de données possible (nb_mesures décroissant)
  arrange(taux_na, desc(nb_mesures))

# Afficher le Top 5 pour choisir manuellement
print(head(classement_stations, 10))

# ==============================================================================
# 3. ANALYSE APPROFONDIE SUR LA CHAMPIONNE (Ex: Lunel-Viel)
# ==============================================================================
# Une fois la championne identifiée dans le tableau ci-dessus, on l'isole.
nom_championne <- classement_stations$nom_station[1] # Prend la 1ère du classement automatiquement
print(paste("Analyse lancée sur la championne :", nom_championne))

# a. Isolation & Nettoyage
df_championne <- df %>%
  filter(nom_station == nom_championne, nom_polluant == target_polluant) %>%
  mutate(date_fin = ymd_hms(date_fin, quiet=TRUE)) %>%
  arrange(date_fin)

# Création grille temporelle parfaite (H-24)
grille <- data.frame(date_fin = seq(min(df_championne$date_fin), max(df_championne$date_fin), by="hour"))
df_clean <- merge(grille, df_championne, by="date_fin", all.x=TRUE)
valeurs_propres <- na.approx(df_clean$valeur, rule=2) # Interpolation

# b. Création Time Series
ts_champ <- ts(valeurs_propres, frequency = 24)

# c. Tests Visuels (ACF/PACF)
# C'est ici que tu vérifies la saisonnalité et les lags
ggtsdisplay(diff(ts_champ, lag=24), main="ACF/PACF sur série différenciée (Lag 24)")

# d. Test du Modèle SARIMA (Celui qu'on a validé : 2,0,0 - 0,1,1)
fit_sarima <- Arima(ts_champ, order=c(2,0,0), seasonal=list(order=c(0,1,1), period=24))
summary(fit_sarima)
checkresiduals(fit_sarima)

# e. Test du GARCH sur résidus
residus <- residuals(fit_sarima)
# Test ARCH (McLeod-Li)
print(Box.test(residus^2, type="Ljung-Box", lag=24))

# Fit GARCH
spec_garch <- ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)),
                         mean.model=list(armaOrder=c(0,0), include.mean=FALSE))
fit_garch <- ugarchfit(spec=spec_garch, data=residus)

print(fit_garch)

# Si tout est bon ici, tu peux passer au script 2 (Backend) !
