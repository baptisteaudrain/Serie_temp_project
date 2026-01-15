eda_stats <- serie_reference %>%
  group_by(polluant) %>%
  summarise(
    n = n(),
    min = min(valeur),
    q05 = quantile(valeur, 0.05),
    median = median(valeur),
    mean = mean(valeur),
    q95 = quantile(valeur, 0.95),
    max = max(valeur),
    sd = sd(valeur)
  )

eda_stats


ggplot(serie_reference, aes(x = date_h, y = valeur)) +
  geom_line() +
  facet_wrap(~ polluant, scales = "free_y", ncol = 1) +
  labs(
    title = "Hourly air pollution time series (reference series)",
    x = "Date",
    y = "Concentration"
  ) +
  theme_minimal()

stl_decompose <- function(df, freq = 24) {
  ts_obj <- ts(df$valeur, frequency = freq)
  stl(ts_obj, s.window = "periodic", robust = TRUE)
}

stl_o3   <- stl_decompose(filter(serie_reference, polluant == "O3"))
stl_no2  <- stl_decompose(filter(serie_reference, polluant == "NO2"))
stl_PM2.5 <- stl_decompose(filter(serie_reference, polluant == "PM2.5"))

par(mfrow = c(3,1))
plot(stl_o3,   main = "STL decomposition – O3 (rural mean)")
plot(stl_no2,  main = "STL decomposition – NO2 (rural)")
plot(stl_PM2.5, main = "STL decomposition – PM2.5 (Peyrusse-Vieille)")
par(mfrow = c(1,1))


serie_reference %>%
  mutate(hour = hour(date_h)) %>%
  group_by(polluant, hour) %>%
  summarise(valeur_moy = mean(valeur), .groups = "drop") %>%
  ggplot(aes(x = hour, y = valeur_moy)) +
  geom_line() +
  facet_wrap(~ polluant, scales = "free_y") +
  labs(
    title = "Average diurnal cycle",
    x = "Hour of day",
    y = "Mean concentration"
  ) +
  theme_minimal()


par(mfrow = c(3,2))
acf(filter(serie_reference, polluant == "O3")$valeur,  main = "ACF – O3")
pacf(filter(serie_reference, polluant == "O3")$valeur, main = "PACF – O3")

acf(filter(serie_reference, polluant == "NO2")$valeur,  main = "ACF – NO2")
pacf(filter(serie_reference, polluant == "NO2")$valeur, main = "PACF – NO2")

acf(filter(serie_reference, polluant == "PM2.5")$valeur,  main = "ACF – PM2.5")
pacf(filter(serie_reference, polluant == "PM2.5")$valeur, main = "PACF – PM2.5")
par(mfrow = c(1,1))



detect_outliers <- function(df) {
  med <- median(df$valeur)
  mad_val <- mad(df$valeur)
  
  df %>%
    mutate(
      z_robust = (valeur - med) / mad_val,
      outlier = abs(z_robust) > 5
    )
}

serie_reference_outliers <- serie_reference %>%
  group_by(polluant) %>%
  group_modify(~ detect_outliers(.x)) %>%
  ungroup()

# Nombre d'anomalies
serie_reference_outliers %>%
  group_by(polluant) %>%
  summarise(nb_outliers = sum(outlier))

ggplot(serie_reference_outliers,
       aes(x = date_h, y = valeur)) +
  geom_line(color = "steelblue") +
  geom_point(
    data = subset(serie_reference_outliers, outlier),
    aes(x = date_h, y = valeur),
    color = "red",
    size = 1.4,
    alpha = 0.8
  ) +
  facet_wrap(~ polluant, scales = "free_y", ncol = 1) +
  labs(
    title = "Reference time series with detected outliers",
    subtitle = "Outliers identified using a robust z-score (MAD)",
    x = "Date",
    y = "Concentration"
  ) +
  theme_minimal()

