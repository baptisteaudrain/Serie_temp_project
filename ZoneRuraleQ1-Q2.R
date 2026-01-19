plot_series_commune <- function(commune) {
  
  s <- serie_ville %>%
    filter(nom_com == commune) %>%
    arrange(date_h)
  
  if (nrow(s) == 0) stop("Aucune donnée pour cette commune.")
  
  ggplot(s, aes(x = date_h, y = valeur)) +
    geom_line(color = "steelblue") +
    facet_wrap(~ nom_polluant, ncol = 1, scales = "free_y") +
    labs(
      title = paste("Séries horaires –", commune, "(zones rurales)"),
      x = "Date",
      y = "Concentration"
    ) +
    theme_minimal()
}

# Exemple
plot_series_commune("PEYRUSSE-VIEILLE")

n_series <- serie_ville %>%
  distinct(nom_com, nom_polluant) %>%
  nrow()

n_series

# ============================
# Série moyenne par polluant
# ============================

serie_polluant_moyenne <- serie_ville %>%
  group_by(nom_polluant, date_h) %>%
  summarise(
    valeur_moyenne = mean(valeur, na.rm = TRUE),
    nb_communes = n(),        # combien de villes contribuent à la moyenne
    .groups = "drop"
  ) %>%
  arrange(nom_polluant, date_h)

# Vérification
head(serie_polluant_moyenne)
ggplot(serie_polluant_moyenne,
       aes(x = date_h, y = valeur_moyenne)) +
  geom_line(color = "steelblue") +
  facet_wrap(~ nom_polluant, scales = "free_y", ncol = 1) +
  labs(
    title = "Séries horaires moyennes par polluant (zones rurales)",
    x = "Date",
    y = "Concentration moyenne"
  ) +
  theme_minimal()


## Comparaison des séries par rapport aux moyennes

library(tidyverse)
install.packages("forecast")
library(forecast)

# Sécurité : on vérifie que la série moyenne existe
stopifnot(exists("serie_polluant_moyenne"))
stopifnot(exists("serie_ville"))

# Fonction utilitaire : aligner deux séries sur les mêmes timestamps
align_series <- function(df1, df2, value1, value2) {
  df <- inner_join(df1, df2, by = "date_h")
  list(
    x = df[[value1]],
    y = df[[value2]]
  )
}

compare_to_mean <- function(ville, polluant,
                            serie_ville,
                            serie_polluant_moyenne,
                            lag_max = 48) {
  
  # Série locale
  s_loc <- serie_ville %>%
    filter(nom_com == ville, nom_polluant == polluant) %>%
    select(date_h, valeur)
  
  # Série moyenne du polluant
  s_mean <- serie_polluant_moyenne %>%
    filter(nom_polluant == polluant) %>%
    select(date_h, valeur_moyenne)
  
  if (nrow(s_loc) < 200) {
    warning(paste("Série courte pour", ville, polluant))
  }
  
  # Alignement temporel
  aligned <- align_series(
    s_loc, s_mean,
    value1 = "valeur",
    value2 = "valeur_moyenne"
  )
  
  x <- aligned$x
  y <- aligned$y
  
  # --------------------
  # 1) Corrélation
  # --------------------
  corr <- cor(x, y)
  
  # --------------------
  # 2) RMSE
  # --------------------
  rmse <- sqrt(mean((x - y)^2))
  
  # --------------------
  # 3) Test KS (distribution)
  # --------------------
  ks_pvalue <- suppressWarnings(ks.test(x, y)$p.value)
  
  # --------------------
  # 4) Cross-correlation (max absolu)
  # --------------------
  ccf_vals <- ccf(x, y, lag.max = lag_max, plot = FALSE)
  max_ccf <- max(abs(ccf_vals$acf))
  
  # --------------------
  # 5) FARIMA (d)
  # --------------------
  d_loc  <- tryCatch(arfima(x)$d, error = function(e) NA)
  d_mean <- tryCatch(arfima(y)$d, error = function(e) NA)
  
  tibble(
    commune = ville,
    polluant = polluant,
    n_obs = length(x),
    corr = corr,
    rmse = rmse,
    ks_pvalue = ks_pvalue,
    max_ccf = max_ccf,
    d_local = d_loc,
    d_moyen = d_mean,
    abs_diff_d = abs(d_loc - d_mean)
  )
}

# Liste des séries locales existantes
series_list <- serie_ville %>%
  distinct(nom_com, nom_polluant)

# Boucle complète
results_comparison <- series_list %>%
  pmap_dfr(function(nom_com, nom_polluant) {
    compare_to_mean(
      ville = nom_com,
      polluant = nom_polluant,
      serie_ville = serie_ville,
      serie_polluant_moyenne = serie_polluant_moyenne
    )
  })

results_comparison

## We have seen it's not well working for PM2.5, so we will shearsh further

library(tidyverse)
library(lubridate)
library(forecast)

decompose_PM2.5 <- function(commune) {
  
  s <- serie_ville %>%
    filter(nom_com == commune, nom_polluant == "PM2.5") %>%
    arrange(date_h)
  
  if (nrow(s) == 0) stop("Aucune série PM2.5 pour cette commune.")
  
  # Série temporelle horaire
  # Saisonnalité journalière : 24
  ts_PM2.5 <- ts(s$valeur, frequency = 24)
  
  # Décomposition STL (robuste aux outliers)
  stl_fit <- stl(ts_PM2.5, s.window = "periodic", robust = TRUE)
  
  list(
    commune = commune,
    stl = stl_fit,
    data = s
  )
}

PM2.5_bolquere <- decompose_PM2.5("BOLQUERE")
plot(PM2.5_bolquere$stl,
     main = "STL decomposition – PM2.5 – BOLQUERE")

PM2.5_peyrusse <- decompose_PM2.5("PEYRUSSE-VIEILLE")
plot(PM2.5_peyrusse$stl,
     main = "STL decomposition – PM2.5 – PEYRUSSE-VIEILLE")
library(forecast)

# ----------------------------
# 1) Alignement déjà fait
# ----------------------------
x <- trend_aligned$trend_bol
y <- trend_aligned$trend_pey

# Sécurité : enlever NA éventuels
idx <- complete.cases(x, y)
x <- x[idx]
y <- y[idx]

# ----------------------------
# 2) Calcul des tests
# ----------------------------

# Corrélation
cor_trend <- cor(x, y)

# RMSE
rmse_trend <- sqrt(mean((x - y)^2))

# KS test
ks_trend <- ks.test(x, y)

# Cross-correlation
ccf_trend <- ccf(x, y, lag.max = 168, plot = FALSE)
max_ccf_trend <- max(abs(ccf_trend$acf))

# FARIMA (paramètre d)
d_trend_bol <- arfima(x)$d
d_trend_pey <- arfima(y)$d

# ----------------------------
# 3) Tableau récapitulatif
# ----------------------------
trend_results_PM2.5 <- tibble(
  series_1 = "PM2.5_BOLQUERE_trend",
  series_2 = "PM2.5_PEYRUSSE-VIEILLE_trend",
  n_obs = length(x),
  correlation = cor_trend,
  rmse = rmse_trend,
  ks_pvalue = ks_trend$p.value,
  max_ccf = max_ccf_trend,
  d_bolquere = d_trend_bol,
  d_peyrusse = d_trend_pey,
  abs_diff_d = abs(d_trend_bol - d_trend_pey)
)

trend_results_PM2.5

# ----------------------------
# Série moyenne PM2.5 périurbaine
# ----------------------------

df_peri_PM2.5 <- df %>%
  filter(
    typologie == "Périurbaine",
    nom_polluant == "PM2.5",
    !is.na(valeur),
    valeur >= 0
  ) %>%
  mutate(date_h = floor_date(date_debut, "hour"))

serie_PM2.5_peri_moy <- df_peri_PM2.5 %>%
  group_by(date_h) %>%
  summarise(
    valeur_moyenne = mean(valeur, na.rm = TRUE),
    nb_stations = n(),
    .groups = "drop"
  ) %>%
  arrange(date_h)

# Vérification rapide
summary(serie_PM2.5_peri_moy$valeur_moyenne)

PM2.5_bol <- serie_ville %>%
  filter(nom_com == "BOLQUERE", nom_polluant == "PM2.5") %>%
  select(date_h, valeur)

PM2.5_pey <- serie_ville %>%
  filter(nom_com == "PEYRUSSE-VIEILLE", nom_polluant == "PM2.5") %>%
  select(date_h, valeur)

compare_to_peri_mean <- function(local_df, peri_df, lag_max = 168) {
  
  df_aligned <- inner_join(
    local_df,
    peri_df,
    by = "date_h"
  )
  
  x <- df_aligned$valeur
  y <- df_aligned$valeur_moyenne
  
  idx <- complete.cases(x, y)
  x <- x[idx]
  y <- y[idx]
  
  tibble(
    n_obs = length(x),
    correlation = cor(x, y),
    rmse = sqrt(mean((x - y)^2)),
    ks_pvalue = ks.test(x, y)$p.value,
    max_ccf = max(abs(ccf(x, y, lag.max = lag_max, plot = FALSE)$acf)),
    d_local = arfima(x)$d,
    d_peri = arfima(y)$d,
    abs_diff_d = abs(arfima(x)$d - arfima(y)$d)
  )
}

res_bol_peri <- compare_to_peri_mean(
  local_df = PM2.5_bol,
  peri_df  = serie_PM2.5_peri_moy
)

res_pey_peri <- compare_to_peri_mean(
  local_df = PM2.5_pey,
  peri_df  = serie_PM2.5_peri_moy
)

res_bol_peri$commune <- "BOLQUERE"
res_pey_peri$commune <- "PEYRUSSE-VIEILLE"

results_PM2.5_peri <- bind_rows(res_bol_peri, res_pey_peri)

results_PM2.5_peri


serie_ref_o3 <- serie_polluant_moyenne %>%
  filter(nom_polluant == "O3") %>%
  select(date_h, valeur = valeur_moyenne) %>%
  mutate(polluant = "O3")
serie_ref_no2 <- serie_ville %>%
  filter(nom_polluant == "NO2") %>%
  select(date_h, valeur) %>%
  mutate(polluant = "NO2")

serie_ref_PM2.5 <- serie_ville %>%
  filter(
    nom_com == "PEYRUSSE-VIEILLE",
    nom_polluant == "PM2.5"
  ) %>%
  select(date_h, valeur) %>%
  mutate(polluant = "PM2.5")
serie_reference <- bind_rows(
  serie_ref_o3,
  serie_ref_no2,
  serie_ref_PM2.5
) %>%
  arrange(polluant, date_h)
