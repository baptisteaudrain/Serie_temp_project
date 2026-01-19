# ============================================================
# COMPLETE PIPELINE (NO FUSS) — SPLIT + ACF/PACF + TRAIN/VAL
# Models per pollutant: SARIMA (tuned with candidates), FARIMA, ARMA-GARCH
# Pollutants expected: O3, NO2, PM2.5  (you said you switched to PM2.5)
#
# INPUT REQUIRED:
#   serie_reference : data.frame/tibble with columns
#       - date_h   (POSIXct, hourly)
#       - polluant (chr)
#       - valeur   (num)
#
# OUTPUT:
#   splits              : train/val/test lists per pollutant
#   sarima_results      : table of candidate SARIMA scores per pollutant
#   best_sarima         : best SARIMA (val RMSE) per pollutant
#   farima_results      : FARIMA scores per pollutant (fast via arfima)
#   garch_results       : best ARMA-GARCH per pollutant (small grid)
#   rmse_table_all      : comparison table (SARIMA vs FARIMA vs ARMA-GARCH) per pollutant
# ============================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(forecast)
  library(tseries)
  library(fracdiff)
  library(rugarch)
})

# ----------------------------
# 0) Sanity checks
# ----------------------------
stopifnot(exists("serie_reference"))
stopifnot(all(c("date_h","polluant","valeur") %in% names(serie_reference)))

serie_reference <- serie_reference %>%
  mutate(
    date_h = as.POSIXct(date_h, tz = "UTC"),
    polluant = as.character(polluant),
    valeur = as.numeric(valeur)
  ) %>%
  filter(!is.na(date_h), is.finite(valeur)) %>%
  arrange(polluant, date_h)

polluants_ref <- c("O3", "NO2", "PM2.5")
if (!all(polluants_ref %in% unique(serie_reference$polluant))) {
  message("Available pollutants in serie_reference: ",
          paste(sort(unique(serie_reference$polluant)), collapse = ", "))
  stop("serie_reference does not contain all required pollutants: O3, NO2, PM2.5")
}

# ----------------------------
# 1) Chronological split
# ----------------------------
split_time_series <- function(df, train_prop = 0.70, val_prop = 0.15) {
  df <- df %>% arrange(date_h)
  n <- nrow(df)
  idx_train <- floor(train_prop * n)
  idx_val   <- floor((train_prop + val_prop) * n)
  list(
    train = df[1:idx_train, ],
    val   = df[(idx_train + 1):idx_val, ],
    test  = df[(idx_val + 1):n, ]
  )
}

splits <- serie_reference %>%
  group_by(polluant) %>%
  group_map(~ split_time_series(.x), .keep = TRUE)

names(splits) <- unique(serie_reference$polluant)

# Keep only required pollutants and keep order stable
splits <- splits[polluants_ref]
stopifnot(all(polluants_ref %in% names(splits)))

# Quick split report
split_report <- lapply(splits, function(s) {
  tibble(
    train = nrow(s$train),
    val   = nrow(s$val),
    test  = nrow(s$test),
    start_train = min(s$train$date_h),
    end_train   = max(s$train$date_h),
    start_val   = min(s$val$date_h),
    end_val     = max(s$val$date_h),
    start_test  = min(s$test$date_h),
    end_test    = max(s$test$date_h)
  )
})
print(split_report)

# ----------------------------
# 2) Helpers
# ----------------------------
rmse <- function(y, yhat) sqrt(mean((y - yhat)^2, na.rm = TRUE))
make_ts <- function(x, freq = 24) ts(as.numeric(x), frequency = freq)

# ----------------------------
# 3) ACF/PACF plots (TRAIN)
# ----------------------------
plot_acf_pacf_train <- function(split, polluant, seasonal_period = 24, max_lag = 7*24) {
  x <- as.numeric(split$train$valeur)
  x <- x[is.finite(x)]
  
  op <- par(mfrow = c(2,2))
  on.exit(par(op), add = TRUE)
  
  acf(x, lag.max = max_lag, main = paste("ACF (train) -", polluant))
  pacf(x, lag.max = max_lag, main = paste("PACF (train) -", polluant))
  
  acf(x, lag.max = max_lag, main = paste("ACF (train, zoom seasonal) -", polluant))
  abline(v = seasonal_period, lty = 2)
  pacf(x, lag.max = max_lag, main = paste("PACF (train, zoom seasonal) -", polluant))
  abline(v = seasonal_period, lty = 2)
}

# Run these when you want to inspect quickly:
# plot_acf_pacf_train(splits$O3, "O3")
# plot_acf_pacf_train(splits$NO2, "NO2")
# plot_acf_pacf_train(splits$`PM2.5`, "PM2.5")

# ----------------------------
# 4) SARIMA with fixed candidate list (FAST + REPORTABLE)
# ----------------------------
fit_validate_sarima_candidates <- function(split,
                                           seasonal_period = 24,
                                           candidates,
                                           method = "ML") {
  y_tr <- make_ts(split$train$valeur, freq = seasonal_period)
  y_va <- as.numeric(split$val$valeur)
  h <- length(y_va)
  
  out <- vector("list", length(candidates))
  
  for (i in seq_along(candidates)) {
    cand <- candidates[[i]]
    ord  <- cand$order
    seas <- cand$seasonal
    
    fit <- try(
      Arima(y_tr,
            order = ord,
            seasonal = list(order = seas, period = seasonal_period),
            include.mean = TRUE,
            method = method),
      silent = TRUE
    )
    
    if (inherits(fit, "try-error")) {
      out[[i]] <- tibble(
        p = ord[1], d = ord[2], q = ord[3],
        P = seas[1], D = seas[2], Q = seas[3],
        aic = NA_real_, rmse_val = NA_real_,
        ok = FALSE
      )
      next
    }
    
    fc <- forecast(fit, h = h)
    yhat <- as.numeric(fc$mean)
    
    out[[i]] <- tibble(
      p = ord[1], d = ord[2], q = ord[3],
      P = seas[1], D = seas[2], Q = seas[3],
      aic = AIC(fit),
      rmse_val = rmse(y_va, yhat),
      ok = TRUE
    )
  }
  
  bind_rows(out) %>%
    filter(ok) %>%
    arrange(rmse_val, aic)
}

# Default compact candidate set (edit after you look at ACF/PACF)
sarima_candidates_default <- list(
  list(order = c(1,0,1), seasonal = c(1,0,1)),
  list(order = c(2,0,1), seasonal = c(1,0,1)),
  list(order = c(1,0,2), seasonal = c(1,0,1)),
  list(order = c(1,1,1), seasonal = c(1,0,1)),
  list(order = c(1,0,1), seasonal = c(1,1,1)),
  list(order = c(2,1,1), seasonal = c(1,0,1)),
  list(order = c(1,1,2), seasonal = c(1,0,1))
)

sarima_tuning <- list(
  O3 = sarima_candidates_default,
  NO2 = sarima_candidates_default,
  `PM2.5` = sarima_candidates_default
)

sarima_results <- list()
for (pol in polluants_ref) {
  cat("\n====================\nSARIMA tuning:", pol, "\n====================\n")
  sarima_results[[pol]] <- fit_validate_sarima_candidates(
    split = splits[[pol]],
    seasonal_period = 24,
    candidates = sarima_tuning[[pol]],
    method = "ML"
  )
  print(head(sarima_results[[pol]], 10))
}

best_sarima <- map_dfr(polluants_ref, function(pol) {
  sarima_results[[pol]] %>%
    slice(1) %>%
    mutate(polluant = pol, model = "SARIMA") %>%
    select(polluant, model, p,d,q,P,D,Q, rmse_val, aic)
})
cat("\n--- Best SARIMA per pollutant ---\n")
print(best_sarima)


extract_d_arfima <- function(fit) {
  if (!is.null(fit$dfrac)) return(as.numeric(fit$dfrac))
  if (!is.null(fit$d))     return(as.numeric(fit$d))
  if (!is.null(fit$fit$d)) return(as.numeric(fit$fit$d))
  if (!is.null(fit$model$d)) return(as.numeric(fit$model$d))
  
  # dernier recours : chercher un élément nommé "d" dans la liste
  cand <- unlist(fit, recursive = TRUE, use.names = TRUE)
  idx <- grep("(^|\\.)d($|\\.)|dfrac", names(cand))
  if (length(idx) > 0) return(as.numeric(cand[idx[1]]))
  
  NA_real_
}
# ----------------------------
# 5) FARIMA (fast + stable) via forecast::arfima
# ----------------------------
fit_validate_farima_arfima <- function(split) {
  x_tr <- as.numeric(split$train$valeur)
  x_va <- as.numeric(split$val$valeur)
  
  x_tr <- x_tr[is.finite(x_tr)]
  x_va <- x_va[is.finite(x_va)]
  h <- length(x_va)
  
  fit <- forecast::arfima(x_tr)
  fc <- forecast::forecast(fit, h = h)
  yhat <- as.numeric(fc$mean)
  
  list(
    model = fit,
    yhat_val = yhat,
    rmse_val = rmse(x_va, yhat),
    d = extract_d_arfima(fit)
  )
}


farima_results <- list()
for (pol in polluants_ref) {
  cat("\n====================\nFARIMA (arfima) :", pol, "\n====================\n")
  farima_results[[pol]] <- fit_validate_farima_arfima(splits[[pol]])
  cat("d =", farima_results[[pol]]$d, " | RMSE val =", farima_results[[pol]]$rmse_val, "\n")
}

# ----------------------------
# 6) ARMA-GARCH (small grid to avoid 40min)
# ----------------------------
fit_validate_arma <- function(split,
                              arma_grid = list(p = 0:2, q = 0:2),
                              seasonal_period = 24) {
  
  y_tr <- make_ts(split$train$valeur, freq = seasonal_period)
  y_va <- as.numeric(split$val$valeur)
  h <- length(y_va)
  
  results <- list()
  
  for (p in arma_grid$p) {
    for (q in arma_grid$q) {
      
      fit <- try(
        Arima(
          y_tr,
          order = c(p, 0, q),   # ARMA(p,q)
          include.mean = TRUE,
          method = "ML"
        ),
        silent = TRUE
      )
      
      if (inherits(fit, "try-error")) next
      
      fc <- forecast(fit, h = h)
      yhat <- as.numeric(fc$mean)
      
      results[[paste0("ARMA(",p,",",q,")")]] <- tibble(
        p = p,
        q = q,
        aic = AIC(fit),
        rmse_val = rmse(y_va, yhat)
      )
    }
  }
  
  bind_rows(results) %>%
    arrange(rmse_val, aic)
}

arma_results <- list()

for (pol in names(splits)) {
  cat("\n====================\nARMA tuning:", pol, "\n====================\n")
  
  arma_results[[pol]] <- fit_validate_arma(
    split = splits[[pol]],
    arma_grid = list(p = 0:2, q = 0:2)
  )
  
  print(arma_results[[pol]])
}

best_arma <- map_dfr(names(arma_results), function(pol) {
  arma_results[[pol]] %>%
    slice(1) %>%
    mutate(polluant = pol, model = "ARMA") %>%
    select(polluant, model, p, q, rmse_val, aic)
})

best_arma
# ----------------------------
# 7) RMSE comparison table (ALL MODELS, ALL POLLUTANTS)
# ----------------------------
rmse_table_all <- map_dfr(polluants_ref, function(pol) {
  tibble(
    polluant = pol,
    model = c("SARIMA", "FARIMA", "ARMA-GARCH"),
    rmse_val = c(
      best_sarima %>% filter(polluant == pol) %>% pull(rmse_val),
      farima_results[[pol]]$rmse_val,
      garch_results[[pol]]$rmse_val
    )
  )
}) %>% arrange(polluant, rmse_val)

cat("\n====================\nRMSE validation — model comparison\n====================\n")
print(rmse_table_all)

best_models <- rmse_table_all %>%
  group_by(polluant) %>%
  slice_min(rmse_val, n = 1, with_ties = FALSE) %>%
  ungroup()

cat("\n====================\nBest model per pollutant (by RMSE val)\n====================\n")
print(best_models)
