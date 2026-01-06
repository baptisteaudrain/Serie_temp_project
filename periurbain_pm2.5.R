df <- read.csv("mesure_horaire_view.csv")
summary(df)
df$datetime_fin <- as.POSIXct(df$date_fin, 
                                format="%Y-%m-%d %H:%M:%S", 
                                tz="Europe/Paris")
summary(df$date_fin)


library(dplyr)

df_toulouse_urbain <- df %>% 
  filter(code_station == 'FR50030')

df_toulouse <- df %>% 
  filter(nom_com == 'TOULOUSE')

# Pour voir la liste des polluants uniques


unique(df_toulouse$nom_polluant)
unique(df_toulouse_urbain$nom_polluant)

df %>%
  filter(nom_polluant == 'SO2') %>% distinct(nom_com)

df %>%
  filter(nom_polluant == "H2S"  ) %>% distinct(nom_com)

df %>% 
  filter(nom_com == 'SAINT-GAUDENS') %>% distinct(nom_polluant)


nom_commune <- df %>% 
  distinct(nom_com)

# --------------------------------------

df_toulouse_pm25 <- df %>% 
  filter(nom_com == 'TOULOUSE', nom_polluant == 'PM2.5')

tableau_polluants_par_station <- df %>%
  filter(nom_com == "TOULOUSE") %>%
  group_by(nom_station) %>%
  summarise(
    nb_polluants_distincts = n_distinct(nom_polluant),
    liste_polluants = paste(unique(nom_polluant), collapse = ", ")
  )

print(tableau_polluants_par_station)


# -----------------

# ==============================================================================
# ÉTAPE 0 : Chargement des paquets
# ==============================================================================
library(dplyr)
library(ggplot2)
library(lubridate)
library(zoo) # Pour l'interpolation (na.approx)

# ==============================================================================
# ÉTAPE 1 : Nettoyage et Préparation des Données
# ==============================================================================
# On crée un dataframe propre spécifiquement pour cette analyse
df_berthelot <- df %>%
  # 1. Filtrage : Station et Polluant
  filter(nom_station == "Toulouse-Berthelot Urbain",
         nom_polluant == "PM2.5") %>%
  
  # 2. Gestion des Dates (Format intelligent)
  # ymd_hms est plus sûr si tu as des heures. Si tes données n'ont pas d'heures, utilise ymd()
  mutate(date_fin = ymd_hms(date_fin, quiet = TRUE)) %>% 
  
  # Sécurité : Si ymd_hms échoue (cas où il n'y a pas d'heure), on tente ymd
  mutate(date_fin = if_else(is.na(date_fin), ymd(date_fin), date_fin)) %>%
  
  # 3. Tri chronologique (Indispensable pour les séries temporelles)
  arrange(date_fin) %>% 
  
  # 4. Gestion des valeurs manquantes (Interpolation linéaire)
  # On crée 'valeur_clean' pour boucher les trous (NA) sans supprimer les lignes
  mutate(valeur_clean = na.approx(valeur, rule = 2))

# Petit check de contrôle
print(paste("Nombre de NA restants :", sum(is.na(df_berthelot$valeur_clean))))

# ==============================================================================
# ÉTAPE 2 : Visualisation Moderne (ggplot2)
# ==============================================================================
# On récupère l'unité pour l'affichage
mon_unite <- unique(df_berthelot$unite)[1]

ggplot(df_berthelot, aes(x = date_fin, y = valeur_clean)) +
  geom_line(color = "steelblue", linewidth = 0.6) +
  labs(
    title = "Évolution des PM2.5 - Toulouse Berthelot",
    subtitle = "Données nettoyées (Interpolées)",
    x = "Temps",
    y = paste("Concentration (", mon_unite, ")")
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

# ==============================================================================
# ÉTAPE 3 : Création de l'objet Time Series (ts) pour ARIMA
# ==============================================================================

# --- Calcul dynamique du point de départ ---
date_depart <- min(df_berthelot$date_fin)
annee_start <- year(date_depart)

# On détermine si c'est des données HORAIRES ou JOURNALIÈRES
# Si on a plus de 1 point par jour en moyenne, on traite comme horaire
est_horaire <- (nrow(df_berthelot) / as.numeric(max(df_berthelot$date_fin) - min(df_berthelot$date_fin))) > 1.5

if (est_horaire) {
  # CAS 1 : DONNÉES HORAIRES
  print("Détection : Données HORAIRES")
  
  # Calcul du "numéro de l'heure" dans l'année pour le start
  # (Jour de l'année - 1) * 24 + Heure du jour
  heure_start_index <- (yday(date_depart) - 1) * 24 + hour(date_depart)
  
  ts_berthelot <- ts(
    df_berthelot$valeur_clean, 
    start = c(annee_start, heure_start_index), 
    frequency = 24 * 365.25 # Fréquence annuelle horaire (~8766)
  )
  
} else {
  # CAS 2 : DONNÉES JOURNALIÈRES
  print("Détection : Données JOURNALIÈRES")
  
  jour_start_index <- yday(date_depart)
  
  ts_berthelot <- ts(
    df_berthelot$valeur_clean, 
    start = c(annee_start, jour_start_index), 
    frequency = 365 # Fréquence annuelle journalière
  )
}

# ==============================================================================
# ÉTAPE 4 : Vérification finale
# ==============================================================================
# Le graphique moche de base R pour vérifier que l'axe X affiche bien les années
plot.ts(ts_berthelot, 
        main = "Vérification de l'objet TS (Axe X = Années ?)", 
        ylab = "PM2.5", 
        col = "darkred")





library(dplyr)

# 1. Lister les typologies présentes à Toulouse
df %>% 
  filter(nom_com == 'TOULOUSE') %>% 
  distinct(typologie)

# 2. Recommandé : Voir combien de stations tu as par typologie à Toulouse
# Cela t'aidera à choisir ta station de référence (la "championne")
df %>% 
  filter(nom_com == 'TOULOUSE') %>% 
  group_by(typologie) %>% 
  summarise(nb_stations = n_distinct(nom_station))





##  Zone peri-urbaine ( Périurbaine + Rurale proche Zone Urbaine.)

library(dplyr)

# 1. On définit ta cible : Périurbain & PM2.5
cibles_periurbain <- c("Périurbaine", "Rurale proche Zone Urbaine")

# 2. On génère le classement
classement_periurbain_pm25 <- df %>%
  # Filtre : On ne garde que les lignes qui nous intéressent
  filter(typologie %in% cibles_periurbain,
         nom_polluant == "PM2.5") %>%
  
  # Agrégation par station
  group_by(nom_station, nom_com, typologie) %>%
  summarise(
    nb_mesures = n(), # Nombre total de lignes
    nb_na = sum(is.na(valeur)), # Nombre de trous
    taux_na_pourcent = round(sum(is.na(valeur)) / n() * 100, 2), # % de vide
    date_debut = min(date_fin, na.rm = TRUE), # Début de l'historique
    date_fin = max(date_fin, na.rm = TRUE),   # Fin de l'historique
    .groups = "drop"
  ) %>%
  
  # Tri : D'abord celles qui ont le moins de NA, ensuite celles qui ont le plus de données
  arrange(taux_na_pourcent, desc(nb_mesures))

# 3. Afficher le TOP 5 des candidats
print(head(classement_periurbain_pm25, 5)) 

# ==============================================================================
# 0. CHARGEMENT DES LIBRAIRIES
# ==============================================================================
library(dplyr)
library(lubridate)
library(ggplot2)
library(zoo)        # Pour l'imputation (na.approx)
library(forecast)   # Pour ARIMA
library(tseries)
library(rugarch)    # Pour GARCH

# ==============================================================================
# 1. PRÉPARATION DU DATASET ET NETTOYAGE TEMPOREL
# ==============================================================================

# Filtrage et sélection
df_base <- df %>%
  filter(nom_station == "Lunel-Viel - Industriel", 
         nom_polluant == "PM2.5") %>%
  mutate(date_fin = ymd_hms(date_fin, quiet = TRUE)) %>%
  arrange(date_fin) %>%
  select(date_fin, valeur)

# Création d'une grille temporelle parfaite pour éviter les décalages
grille_complete <- data.frame(
  date_fin = seq(min(df_base$date_fin), max(df_base$date_fin), by = "hour")
)

# Fusion et imputation des données manquantes (Interpolation linéaire)
df_clean <- merge(grille_complete, df_base, by = "date_fin", all.x = TRUE)
df_clean$valeur <- na.approx(df_clean$valeur)

# Création de l'objet Time Series (Fréquence horaire)
ts_periurbain <- ts(df_clean$valeur, frequency = 24)

# ==============================================================================
# 2. MODÉLISATION DE LA MOYENNE (SARIMA)
# ==============================================================================

# Modèle SARIMA(2,0,0)(0,1,1)[24] identifié par analyse ACF/PACF
fit_sarima <- Arima(ts_periurbain, 
                    order = c(2, 0, 0), 
                    seasonal = list(order = c(0, 1, 1), period = 24))

# Résumé et vérification des résidus
summary(fit_sarima)
checkresiduals(fit_sarima)

# Extraction des résidus pour l'étape suivante
residus_sarima <- residuals(fit_sarima)

# ==============================================================================
# 3. DIAGNOSTIC DE LA VOLATILITÉ (EFFET ARCH)
# ==============================================================================

# Visualisation de la volatilité
plot(residus_sarima^2, main = "Carré des résidus (Volatilité)", type = "l", col="darkred")

# Test de McLeod-Li (Ljung-Box sur les carrés)
test_arch <- Box.test(residus_sarima^2, type = "Ljung-Box", lag = 24)
print(test_arch)

# ==============================================================================
# 4. MODÉLISATION DE LA VARIANCE (GARCH) - APPROCHE HYBRIDE
# ==============================================================================

# Spécification GARCH(1,1) sur les résidus du SARIMA
# Mean Model est (0,0) car la moyenne est déjà gérée par le SARIMA
spec_garch <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE), 
  distribution.model = "norm"
)

# Entraînement sur les résidus
fit_garch_final <- ugarchfit(spec = spec_garch, data = residus_sarima)

# Affichage des résultats finaux
print(fit_garch_final)

