## lien de la page web des données : https://www.arcgis.com/home/item.html?id=ed2df250e1e244469efd5d97b61152fd

library(dplyr)
library(forecast)
library(zoo)
library(ggplot2)



### IL FAUT COPIER CES DEUX LIGNES DIRECTEMENT DANS LA CONSOLE SANS LE < DE LA SECONDE LIGNE
## pour charger les données : > url <- "https://atmo-occitanie.maps.arcgis.com/sharing/rest/content/items/ed2df250e1e244469efd5d97b61152fd/data"
## > download.file(url, destfile = "occitanie_pollution.csv", mode="wb")


## mesures_occitanie_72h_poll_princ_1672966166332505238 <- read_csv("mesures_occitanie_72h_poll_princ_-1672966166332505238.csv")

df <- read.csv("occitanie_pollution.csv")

summary(df['date_debut'])

summary(df['nom_polluant'])
unique(df$nom_polluant)


# CREATION DE DATAFRAME POUR CHAQUE POLLUANT
#df_pm25 <- subset(df, nom_polluant == "PM2.5")
#df_nox <- subset(df, nom_polluant == "NOX")
#df_no <- subset(df, nom_polluant == "NO")
#df_no2 <- subset(df, nom_polluant == "NO2")
#df_h2s <- subset(df, nom_polluant == "H2S")
#df_pm10 <- subset(df, nom_polluant == "PM10")
#df_o3 <- subset(df, nom_polluant == "O3")
#df_so2 <- subset(df, nom_polluant == "SO2")



# PM2.5
df_pm25a <- df %>%
  filter(nom_polluant == "PM2.5") %>%
  arrange(date_debut)

# NOX
df_noxa <- df %>%
  filter(nom_polluant == "NOX") %>%
  arrange(date_debut)

# NO
df_noa <- df %>%
  filter(nom_polluant == "NO") %>%
  arrange(date_debut)

# NO2
df_no2a <- df %>%
  filter(nom_polluant == "NO2") %>%
  arrange(date_debut)

# H2S
df_h2sa <- df %>%
  filter(nom_polluant == "H2S") %>%
  arrange(date_debut)

# PM10
df_pm10a <- df %>%
  filter(nom_polluant == "PM10") %>%
  arrange(date_debut)

# O3
df_o3a <- df %>%
  filter(nom_polluant == "O3") %>%
  arrange(date_debut)

# SO2
df_so2a <- df %>%
  filter(nom_polluant == "SO2") %>%
  arrange(date_debut)


# CONVERSION HEURE EN OBJET DATE_TIME

df_pm25a$datetime <- as.POSIXct(df_pm25a$date_debut, 
                               format="%Y-%m-%d %H:%M:%S", 
                               tz="Europe/Paris")



# Vérification
str(df_pm25a)
head(df_pm25a)



############################################################
# PM2.5 à la station 'Toulouse-Berthelot Urbain' - série temporelle
############################################################



df_pm25_toulouse <- df_pm25a %>%
  filter(nom_station == "Toulouse-Berthelot Urbain") %>%
  arrange(date_debut)



df_pm25_toulouse$date_debut <- as.POSIXct(df_pm25_toulouse$date_debut, 
                                          format="%Y-%m-%d %H:%M:%S", 
                                          tz="Europe/Paris")


ts_pm25_toulouse <- ts(df_pm25_toulouse$valeur, frequency = 24)


plot(df_pm25_toulouse$date_debut, df_pm25_toulouse$valeur, type="l",
     xlab="Date/heure", ylab="PM2.5 (µg/m³)", main="PM2.5 - Toulouse")


############################################################
# PM2.5 moyen a toulouse - série temporelle
############################################################



# Filtrer PM2.5 pour Toulouse et trier par date
df_pm25_toulouse <- df %>%
  filter(nom_polluant == "PM2.5", nom_com == 'TOULOUSE') %>%
  arrange(date_debut)

# Convertir date_debut en POSIXct
df_pm25_toulouse$date_debut <- as.POSIXct(df_pm25_toulouse$date_debut, 
                                          format="%Y-%m-%d %H:%M:%S", 
                                          tz="Europe/Paris")

# Calculer la moyenne par heure (agrégation sur toutes les stations)
df_pm25_toulouse_mean <- df_pm25_toulouse %>%
  group_by(date_debut) %>%
  summarise(valeur = mean(valeur, na.rm = TRUE)) %>%
  arrange(date_debut)

# Interpolation pour les valeurs manquantes
df_pm25_toulouse_mean$valeur <- na.approx(df_pm25_toulouse_mean$valeur)

# Créer la série temporelle horaire
ts_pm25_toulouse_mean <- ts(df_pm25_toulouse_mean$valeur, frequency = 24)

# Vérification : plot avec dates réelles
plot(df_pm25_toulouse_mean$date_debut, df_pm25_toulouse_mean$valeur, type="l",
     xlab="Date/heure", ylab="PM2.5 moyen (µg/m³)", main="PM2.5 moyen - Toulouse")

auto.arima(ts_pm25_toulouse_mean)


# 1️⃣ Vérifier la série temporelle
plot(ts_pm25_toulouse_mean, main="PM2.5 moyen - Toulouse", ylab="µg/m³")

# 2️⃣ Ajuster un modèle ARIMA automatique
fit_pm25 <- auto.arima(ts_pm25_toulouse_mean, seasonal = TRUE)

# Résumé du modèle
summary(fit_pm25)

# 3️⃣ Vérifier les résidus
checkresiduals(fit_pm25)  # graphique et tests statistiques

# 4️⃣ Faire des prévisions
# Prévoir les 24 prochaines heures
forecast_pm25 <- forecast(fit_pm25, h = 24)

# 5️⃣ Visualiser les prévisions
autoplot(forecast_pm25) +
  labs(title="Prévision PM2.5 moyen - Toulouse",
       x="Heure", y="PM2.5 (µg/m³)")

# 6️⃣ Option : afficher les valeurs prédites
forecast_pm25_df <- data.frame(
  date = df_pm25_toulouse_mean$date_debut[nrow(df_pm25_toulouse_mean)] + 3600 * (1:24),
  pred = as.numeric(forecast_pm25$mean),
  lower_80 = as.numeric(forecast_pm25$lower[,1]),
  upper_80 = as.numeric(forecast_pm25$upper[,1]),
  lower_95 = as.numeric(forecast_pm25$lower[,2]),
  upper_95 = as.numeric(forecast_pm25$upper[,2])
)

head(forecast_pm25_df)

