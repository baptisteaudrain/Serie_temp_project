# ============================================================
# FULL PIPELINE (ROBUST):
# SARIMA (mean) refit on TRAIN+VAL + GARCH(1,1) on residuals
# Evaluate on TEST for each pollutant.
#
# Requires: splits$O3, splits$NO2, splits$`PM2.5`
# each split: train/val/test with columns date_h, valeur
# ============================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(forecast)
  library(rugarch)
})

rmse <- function(y, yhat) sqrt(mean((y - yhat)^2, na.rm = TRUE))
make_ts <- function(x, freq = 24) ts(as.numeric(x), frequency = freq)

# ----------------------------
# Robust scaling helpers
# ----------------------------
scale_train <- function(x) {
  x <- as.numeric(x)
  mu <- mean(x, na.rm = TRUE)
  s  <- sd(x, na.rm = TRUE)
  if (!is.finite(s) || s < 1e-8) s <- 1
  list(x_sc = (x - mu) / s, mu = mu, sd = s)
}
unscale_vec <- function(x_sc, mu, sd) x_sc * sd + mu

# ----------------------------
# Winsorise for GARCH stability
# ----------------------------
winsorize <- function(x, p = 0.001) {
  lo <- quantile(x, p, na.rm = TRUE)
  hi <- quantile(x, 1 - p, na.rm = TRUE)
  pmin(pmax(x, lo), hi)
}

# ----------------------------
# Final SARIMA specs
# ----------------------------
sarima_specs <- list(
  O3     = list(order = c(1,0,2), seasonal = c(1,0,1), period = 24),
  NO2    = list(order = c(1,0,1), seasonal = c(1,0,1), period = 24),
  `PM2.5`= list(order = c(1,1,2), seasonal = c(1,0,1), period = 24)
)

# ============================================================
# 1) ROBUST SARIMA fit on TRAIN+VAL (with automatic fallbacks)
#    - scales series to stabilize optimization
#    - handles d=1 by using drift (not include.mean)
#    - tries multiple optimizers / methods / transform.pars
# ============================================================
fit_sarima_trainval_robust <- function(split, spec) {
  df_tv <- bind_rows(split$train, split$val) %>% arrange(date_h)
  
  # Clean
  y_raw <- as.numeric(df_tv$valeur)
  if (any(!is.finite(y_raw))) {
    ok <- is.finite(y_raw)
    df_tv <- df_tv[ok, ]
    y_raw <- y_raw[ok]
  }
  if (length(y_raw) < 200) stop("Train+val series too short for SARIMA.")
  
  # Scale for stability
  sc <- scale_train(y_raw)
  y_sc <- sc$x_sc
  y_ts <- make_ts(y_sc, freq = spec$period)
  
  d <- spec$order[2]
  # For d=1, include.mean is not identified; use drift instead.
  include_mean  <- (d == 0)
  include_drift <- (d > 0)
  
  # Fallback grid
  methods_try <- c("CSS-ML", "ML", "CSS")
  optim_try   <- c("BFGS", "Nelder-Mead", "CG")
  transf_try  <- c(TRUE, FALSE)
  
  last_err <- NULL
  
  for (mth in methods_try) {
    for (optm in optim_try) {
      for (trp in transf_try) {
        
        fit <- try(
          Arima(
            y_ts,
            order = spec$order,
            seasonal = list(order = spec$seasonal, period = spec$period),
            include.mean = include_mean,
            include.drift = include_drift,
            method = mth,
            optim.method = optm,
            transform.pars = trp
          ),
          silent = TRUE
        )
        
        if (!inherits(fit, "try-error")) {
          return(list(
            fit = fit,
            trainval = df_tv,
            scale_mu = sc$mu,
            scale_sd = sc$sd
          ))
        } else {
          last_err <- fit
        }
      }
    }
  }
  
  stop("SARIMA fit failed after fallbacks. Last error: ", as.character(last_err))
}

# ============================================================
# 2) ROBUST GARCH fit on SARIMA residuals (train+val)
# ============================================================
fit_garch_on_residuals_robust <- function(residuals_vec,
                                          dist_try = c("std", "norm"),
                                          solvers_try = c("hybrid", "solnp"),
                                          winsor_p = 0.001) {
  e <- as.numeric(residuals_vec)
  e <- e[is.finite(e)]
  if (length(e) < 300) stop("Residual series too short for stable GARCH fit.")
  
  # Center + scale
  e <- e - mean(e, na.rm = TRUE)
  s <- sd(e, na.rm = TRUE)
  if (!is.finite(s) || s < 1e-8) stop("Residual variance too small for GARCH.")
  
  e_sc <- e / s
  e_sc <- winsorize(e_sc, p = winsor_p)
  
  last_err <- NULL
  
  for (dist in dist_try) {
    spec <- ugarchspec(
      mean.model     = list(armaOrder = c(0,0), include.mean = FALSE),
      variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
      distribution.model = dist
    )
    
    for (solver in solvers_try) {
      fit <- try(
        ugarchfit(
          spec,
          data = e_sc,
          solver = solver,
          fit.control = list(stationarity = 1, fixed.se = 0, scale = 1)
        ),
        silent = TRUE
      )
      
      if (!inherits(fit, "try-error")) {
        return(list(fit = fit, resid_sd = s, dist = dist, solver = solver))
      } else {
        last_err <- fit
      }
    }
  }
  
  stop("GARCH fit failed. Last error: ", as.character(last_err))
}

# ============================================================
# 3) Predict on TEST with SARIMA mean + GARCH sigma
#    IMPORTANT: SARIMA fitted on SCALED y -> forecast on scaled,
#    then unscale mu back to original units.
# ============================================================
predict_test_with_sarima_garch <- function(split, sar_obj, garch_obj) {
  df_te <- split$test %>% arrange(date_h)
  y_te <- as.numeric(df_te$valeur)
  H <- nrow(df_te)
  
  # SARIMA forecast in scaled units
  fc_sc <- forecast(sar_obj$fit, h = H)
  mu_sc <- as.numeric(fc_sc$mean)
  
  # Unscale forecast mean to original units
  mu <- unscale_vec(mu_sc, sar_obj$scale_mu, sar_obj$scale_sd)
  
  # Residuals on test (original units)
  e <- y_te - mu
  
  # GARCH forecast sigma for residuals:
  # garch fit was on standardized residuals (e/resid_sd)
  gfc <- ugarchforecast(garch_obj$fit, n.ahead = H)
  sigma_sc <- as.numeric(sigma(gfc))
  
  # Back to original residual scale: sigma_sc * resid_sd
  sigma <- pmax(sigma_sc * garch_obj$resid_sd, 1e-8)
  
  z <- e / sigma
  
  tibble(
    date_h = df_te$date_h,
    y = y_te,
    mu = mu,
    resid = e,
    sigma = sigma,
    z = z,
    abs_z = abs(z)
  )
}

# ============================================================
# RUN
# ============================================================
polluants <- names(sarima_specs)
stopifnot(all(polluants %in% names(splits)))

models_final <- list()
test_scores_list <- list()

for (pol in polluants) {
  cat("\n====================\nFINAL FIT:", pol, "\n====================\n")
  
  # 1) SARIMA robust fit on train+val
  sar_obj <- fit_sarima_trainval_robust(splits[[pol]], sarima_specs[[pol]])
  
  # Residuals from SARIMA on train+val (scaled units!)
  # Convert residuals back to original units before GARCH:
  res_tv_sc <- as.numeric(residuals(sar_obj$fit))
  res_tv <- res_tv_sc * sar_obj$scale_sd
  
  # 2) GARCH robust fit on SARIMA residuals (original units)
  g_obj <- fit_garch_on_residuals_robust(
    residuals_vec = res_tv,
    dist_try = c("std", "norm"),
    solvers_try = c("hybrid", "solnp"),
    winsor_p = 0.001
  )
  cat("GARCH OK | dist =", g_obj$dist, "| solver =", g_obj$solver, "\n")
  
  # 3) Predict on test
  sc <- predict_test_with_sarima_garch(splits[[pol]], sar_obj, g_obj) %>%
    mutate(polluant = pol)
  
  rmse_test <- rmse(sc$y, sc$mu)
  cat("Test RMSE (SARIMA mean):", rmse_test, "\n")
  
  models_final[[pol]] <- list(
    sarima_fit = sar_obj$fit,
    sarima_scale = sar_obj[c("scale_mu","scale_sd")],
    garch_fit = g_obj$fit,
    garch_meta = g_obj[c("resid_sd","dist","solver")],
    rmse_test = rmse_test
  )
  
  test_scores_list[[pol]] <- sc
}

test_scores <- bind_rows(test_scores_list)

# ============================================================
# Thresholds + anomaly flags
# ============================================================
thresholds <- test_scores %>%
  group_by(polluant) %>%
  summarise(
    thr_absz_995 = quantile(abs_z, 0.995, na.rm = TRUE),
    thr_absz_99  = quantile(abs_z, 0.99,  na.rm = TRUE),
    .groups = "drop"
  )

print(thresholds)

test_scores <- test_scores %>%
  left_join(thresholds, by = "polluant") %>%
  mutate(
    anomaly_3sigma = abs_z > 3,
    anomaly_995 = abs_z > thr_absz_995,
    anomaly_99  = abs_z > thr_absz_99
  )

# ============================================================
# Quick plots
# ============================================================
plot_series_with_anomalies <- function(df, pol,
                                       rule = c("anomaly_995","anomaly_3sigma","anomaly_99")) {
  rule <- match.arg(rule)
  d <- df %>% filter(polluant == pol) %>% arrange(date_h)
  
  op <- par(mfrow = c(2,1))
  on.exit(par(op), add = TRUE)
  
  plot(d$date_h, d$y, type = "l",
       main = paste(pol, "- Observed and SARIMA mean forecast (TEST)"),
       xlab = "Time", ylab = "Value")
  lines(d$date_h, d$mu, lty = 2)
  legend("topleft", legend = c("Observed", "Forecast mean"),
         lty = c(1,2), bty = "n")
  
  plot(d$date_h, d$abs_z, type = "l",
       main = paste(pol, "- |z| anomaly score (TEST)"),
       xlab = "Time", ylab = "|z|")
  abline(h = 3, lty = 2)
  points(d$date_h[d[[rule]]], d$abs_z[d[[rule]]], pch = 19, cex = 0.5)
  legend("topright",
         legend = c("|z|", "threshold 3", paste("flag:", rule)),
         lty = c(1,2,NA), pch = c(NA, NA, 19), bty = "n")
}

# Example:
plot_series_with_anomalies(test_scores, "PM2.5", rule = "anomaly_995")

# ============================================================
# Export anomalies if needed
# ============================================================
anomalies <- test_scores %>%
  filter(anomaly_995) %>%
  select(polluant, date_h, y, mu, resid, sigma, z, abs_z)

# write.csv(test_scores, "test_scores_sarima_garch.csv", row.names = FALSE)
# write.csv(anomalies, "anomalies_test_995.csv", row.names = FALSE)

print(head(anomalies, 20))




library(dplyr)
library(forecast)
library(zoo)

extract_trend <- function(split, model_entry, window = 168) {
  # split: splits[[pol]]
  # model_entry: models_final[[pol]] containing sarima_fit + sarima_scale
  
  stopifnot(all(c("sarima_fit", "sarima_scale") %in% names(model_entry)))
  stopifnot(all(c("train", "val", "test") %in% names(split)))
  
  sar_fit <- model_entry$sarima_fit
  mu0 <- model_entry$sarima_scale$scale_mu
  sd0 <- model_entry$sarima_scale$scale_sd
  
  df_tv <- bind_rows(split$train, split$val) %>% arrange(date_h)
  df_te <- split$test %>% arrange(date_h)
  df_all <- bind_rows(df_tv, df_te) %>% arrange(date_h)
  
  n_tv <- nrow(df_tv)
  h_te <- nrow(df_te)
  
  # 1) In-sample fitted (train+val)
  fitted_sc <- as.numeric(fitted(sar_fit))
  fitted_mu <- fitted_sc * sd0 + mu0
  
  # 2) Out-of-sample forecast (test)
  fc_sc <- as.numeric(forecast(sar_fit, h = h_te)$mean)
  fc_mu <- fc_sc * sd0 + mu0
  
  # 3) Combine and align lengths safely
  mu <- c(fitted_mu, fc_mu)
  
  # If something slightly mismatched, force align to df_all
  if (length(mu) != nrow(df_all)) {
    # keep the last n rows (most common fix if fitted has extra)
    if (length(mu) > nrow(df_all)) mu <- tail(mu, nrow(df_all))
    # or pad with NA if too short
    if (length(mu) < nrow(df_all)) mu <- c(mu, rep(NA_real_, nrow(df_all) - length(mu)))
  }
  
  tibble(
    date_h = df_all$date_h,
    mu = mu
  ) %>%
    mutate(
      trend = zoo::rollmean(mu, k = window, fill = NA_real_, align = "center")
    )
}
trend_series <- lapply(names(models_final), function(pol) {
  extract_trend(
    split = splits[[pol]],
    model_entry = models_final[[pol]],
    window = 168
  ) %>% mutate(polluant = pol)
})

trend_df <- bind_rows(trend_series)
plot_trend <- function(trend_df, pol) {
  d <- trend_df %>% filter(polluant == pol)
  
  plot(d$date_h, d$mu, type = "l", col = "grey70",
       main = paste(pol, "- SARIMA signal + smoothed trend"),
       xlab = "Time", ylab = "Concentration")
  lines(d$date_h, d$trend, col = "red", lwd = 2)
  legend("topleft",
         legend = c("SARIMA signal (mu)", "Trend (weekly smooth)"),
         col = c("grey70", "red"), lwd = c(1,2), bty = "n")
}

plot_trend(trend_df, "O3")
plot_trend(trend_df, "NO2")
plot_trend(trend_df, "PM2.5")


trend_summary <- trend_df %>%
  filter(!is.na(trend)) %>%
  group_by(polluant) %>%
  summarise(
    slope_per_year = coef(lm(trend ~ as.numeric(date_h)))[2] * 3600 * 24 * 365,
    direction = case_when(
      slope_per_year > 0 ~ "Increasing",
      slope_per_year < 0 ~ "Decreasing",
      TRUE ~ "Stable"
    ),
    .groups = "drop"
  )

trend_summary

