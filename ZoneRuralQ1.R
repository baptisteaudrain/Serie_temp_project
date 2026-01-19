# ============================
# Q1 — DATA PREPROCESSING
# Environmental data (zones rurales)
# ============================

library(tidyverse)
library(lubridate)
library(stringr)

# ----------------------------
# 0) Paramètres
# ----------------------------
polluants <- c("NO2", "PM2.5", "O3")

date_fin <- as.POSIXct("2026-01-06 12:00:00", tz = "UTC")
date_debut_1an <- date_fin - days(365)

# ----------------------------
# 1) Lecture
# ----------------------------
df_raw <- read.csv("mesure_horaire_view.csv", stringsAsFactors = FALSE)
summary(df_raw)
# ----------------------------
# 2) Parsing dates + conversions
# ----------------------------
df <- df_raw %>%
  mutate(
    date_debut = parse_date_time(
      date_debut,
      orders = c("mdy HMS p", "dmy HMS p", "ymd HMS", "ymd HM"),
      tz = "UTC"
    ),
    date_fin = parse_date_time(
      date_fin,
      orders = c("mdy HMS p", "dmy HMS p", "ymd HMS", "ymd HM"),
      tz = "UTC"
    ),
    valeur = suppressWarnings(as.numeric(str_replace(valeur, ",", ".")))
  ) %>%
  filter(!is.na(date_debut))

# ----------------------------
# 3) Tronquage temporel (1 an)
# ----------------------------
df <- df %>%
  filter(
    date_debut >= date_debut_1an,
    date_debut <= date_fin
  )

cat("Q1 — période couverte:\n")
print(range(df$date_debut))

# ----------------------------
# 4) Filtrage zones rurales + polluants
# ----------------------------

# 1) Vérifier rapidement les colonnes et quelques valeurs
glimpse(df)

# 2) Voir ce qu'il y a vraiment dans typologie / nom_polluant / statut_valid
cat("\n--- typologie (top 30) ---\n")
print(df %>% count(typologie, sort = TRUE) %>% head(30))

cat("\n--- nom_polluant (top 30) ---\n")
print(df %>% count(nom_polluant, sort = TRUE) %>% head(30))

cat("\n--- statut_valid (tous) ---\n")
print(df %>% count(statut_valid, sort = TRUE))
typologies_rurales <- c("Rurale Nationale", "Rurale Régionale")

polluants <- c("NO2", "PM2.5", "O3")

df_rural <- df %>%
  filter(
    typologie %in% typologies_rurales,
    nom_polluant %in% polluants,
    !is.na(valeur),
    valeur >= 0
    # 👉 on enlève volontairement statut_valid pour l’instant
  )

df_rural <- readRDS("df_rural.rds")
cat("Q1 — lignes après filtre rural:", nrow(df_rural), "\n")

# ----------------------------
# 5) Harmonisation horaire + agrégation
# ----------------------------
serie_ville <- df_rural %>%
  mutate(
    date_h = floor_date(date_debut, unit = "hour")
  ) %>%
  group_by(
    nom_com,
    nom_polluant,
    date_h
  ) %>%
  summarise(
    valeur = mean(valeur, na.rm = TRUE),
    n_mesures = n(),
    .groups = "drop"
  ) %>%
  arrange(nom_com, nom_polluant, date_h)

cat("Q1 — lignes après agrégation horaire:", nrow(serie_ville), "\n")

# ----------------------------
# 6) Checks qualité (rapportables)
# ----------------------------

# 6.1 Nombre de villes rurales
nb_villes <- serie_ville %>% distinct(nom_com) %>% nrow()
cat("Q1 — nombre de villes rurales:", nb_villes, "\n")

# 6.2 Couverture temporelle
coverage <- serie_ville %>%
  group_by(nom_com, nom_polluant) %>%
  summarise(
    debut = min(date_h),
    fin = max(date_h),
    nb_heures = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(nb_heures))

print(head(coverage, 10))

# 6.3 Doublons horaires
dup_summary <- serie_ville %>%
  summarise(
    proportion_heures_avec_doublons = mean(n_mesures > 1),
    max_mesures_sur_une_heure = max(n_mesures)
  )

print(dup_summary)

# 6.4 Distribution des valeurs
stats_valeurs <- serie_ville %>%
  group_by(nom_polluant) %>%
  summarise(
    min = min(valeur),
    q01 = quantile(valeur, 0.01),
    median = median(valeur),
    q99 = quantile(valeur, 0.99),
    max = max(valeur)
  )

print(stats_valeurs)

# ----------------------------
# 7) Sauvegarde optionnelle
# ----------------------------
# write.csv(serie_ville, "serie_ville_Q1_clean.csv", row.names = FALSE)

# Objet final Q1:
# - df_rural   : données nettoyées (avant hourly strict)
# - serie_ville: séries horaires propres par ville × polluant
