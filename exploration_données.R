## lien de la page web des données : https://www.arcgis.com/home/item.html?id=ed2df250e1e244469efd5d97b61152fd

library(dplyr)
library(lubridate)
library(zoo)
library(forecast)
library(ggplot2)



### IL FAUT COPIER CES DEUX LIGNES DIRECTEMENT DANS LA CONSOLE SANS LE < DE LA SECONDE LIGNE
## pour charger les données : > url <- "https://atmo-occitanie.maps.arcgis.com/sharing/rest/content/items/ed2df250e1e244469efd5d97b61152fd/data"
## > download.file(url, destfile = "occitanie_pollution.csv", mode="wb")


## mesures_occitanie_72h_poll_princ_1672966166332505238 <- read_csv("mesures_occitanie_72h_poll_princ_-1672966166332505238.csv")

df <- read.csv("mesure_horaire_view.csv")
# -------------------------------------------------------------------
# 1. Préparation commune : dates propres
# -------------------------------------------------------------------
df <- df %>%
  mutate(
    date_debut = ymd_hms(date_debut, quiet = TRUE),
    date_fin   = ymd_hms(date_fin, quiet = TRUE)
  ) %>%
  filter(!is.na(date_debut))

# -------------------------------------------------------------------
# 2. Dataframes par polluant
# -------------------------------------------------------------------
df_pm25 <- df %>% filter(nom_polluant == "PM2.5")
df_o3   <- df %>% filter(nom_polluant == "O3")
df_no2  <- df %>% filter(nom_polluant == "NO2")

# -------------------------------------------------------------------
# 3. Fonction utilitaire : choisir la "meilleure" station
#    Critères :
#    - le plus de points non NA
#    - taux de NA le plus faible
# -------------------------------------------------------------------
choisir_meilleure_station <- function(data_polluant, typologie_cible) {
  
  df_filt <- data_polluant %>%
    filter(typologie == typologie_cible)
  
  if (nrow(df_filt) == 0) return(NULL)
  
  station_choisie <- df_filt %>%
    group_by(nom_station) %>%
    summarise(
      nb_total = n(),
      nb_non_na = sum(!is.na(valeur)),
      na_rate = 1 - nb_non_na / nb_total,
      .groups = "drop"
    ) %>%
    arrange(na_rate, desc(nb_non_na)) %>%
    slice(1) %>%
    pull(nom_station)
  
  return(station_choisie)
}

# -------------------------------------------------------------------
# 4. Fonction utilitaire : construire une série temporelle horaire
#    - on trie par date
#    - on met sur grille horaire complète
#    - interpolation linéaire (na.approx)
# -------------------------------------------------------------------
construire_ts_station <- function(data_polluant, station_name) {
  
  df_s <- data_polluant %>%
    filter(nom_station == station_name) %>%
    arrange(date_debut)
  
  if (nrow(df_s) == 0) return(NULL)
  
  # Grille horaire régulière
  grille <- data.frame(
    date_debut = seq(min(df_s$date_debut), max(df_s$date_debut), by = "hour")
  )
  
  df_merge <- merge(grille, df_s[, c("date_debut", "valeur")],
                    by = "date_debut", all.x = TRUE)
  
  valeurs_impute <- na.approx(df_merge$valeur, rule = 2)
  
  ts(valeurs_impute, frequency = 24)
}

# -------------------------------------------------------------------
# 5. Typologies ciblées
# -------------------------------------------------------------------
typologies_cibles <- c(
  "Périurbaine",
  "Rurale Proche Zone Urbaine",
  "Rurale Régionale",
  "Rurale Nationale"
)

# -------------------------------------------------------------------
# 6. Pour chaque polluant et typologie : choisir station + construire ts
# -------------------------------------------------------------------

# On stocke les séries temporelles dans une liste nommée
ts_list <- list()

for (typ in typologies_cibles) {
  
  # ----- PM2.5 -----
  st_pm25 <- choisir_meilleure_station(df_pm25, typ)
  if (!is.null(st_pm25)) {
    ts_name <- paste0("ts_pm25_", gsub(" ", "_", typ))
    ts_list[[ts_name]] <- construire_ts_station(df_pm25, st_pm25)
    cat("PM2.5 - Typologie:", typ, "-> Station choisie:", st_pm25, "\n")
  } else {
    cat("PM2.5 - Typologie:", typ, "-> Aucune station trouvée\n")
  }
  
  # ----- O3 -----
  st_o3 <- choisir_meilleure_station(df_o3, typ)
  if (!is.null(st_o3)) {
    ts_name <- paste0("ts_o3_", gsub(" ", "_", typ))
    ts_list[[ts_name]] <- construire_ts_station(df_o3, st_o3)
    cat("O3 - Typologie:", typ, "-> Station choisie:", st_o3, "\n")
  } else {
    cat("O3 - Typologie:", typ, "-> Aucune station trouvée\n")
  }
  
  # ----- NO2 -----
  st_no2 <- choisir_meilleure_station(df_no2, typ)
  if (!is.null(st_no2)) {
    ts_name <- paste0("ts_no2_", gsub(" ", "_", typ))
    ts_list[[ts_name]] <- construire_ts_station(df_no2, st_no2)
    cat("NO2 - Typologie:", typ, "-> Station choisie:", st_no2, "\n")
  } else {
    cat("NO2 - Typologie:", typ, "-> Aucune station trouvée\n")
  }
}

# -------------------------------------------------------------------
# 7. (Optionnel) Assigner les séries dans l'environnement global
#    pour avoir directement 12 objets ts_* utilisables
# -------------------------------------------------------------------
for (nom_ts in names(ts_list)) {
  assign(nom_ts, ts_list[[nom_ts]], envir = .GlobalEnv)
}

# Maintenant tu as, quand elles existent :
# ts_pm25_Périurbaine
# ts_pm25_Rurale_Proche_Zone_Urbane
# ts_pm25_Rurale_Régionale
# ts_pm25_Rurale_Nationale
# et idem pour ts_o3_* et ts_no2_*