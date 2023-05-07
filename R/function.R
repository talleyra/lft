library(dplyr)
library(timetk)
library(tidymodels)

flex_forecast <- function(df, .date_var, .value, .n = 20, .l, .ff, .dec, .seasonal_period = 24, .seasonal_window, .trend_window) {

  decomp <- df %>%
    add_components(
      .value = {{ .value }},
      .seasonal_periods = .seasonal_period,
      .seasonal_window = .seasonal_window,
      .trend_window = .trend_window
    )

  future_dttm <- decomp %>%
    tk_index() %>%
    tk_make_future_timeseries(length_out = .l)

  seasonal_carried <- decomp %>%
    slice_tail(n = .seasonal_period) %>%
    pull(seasonal24) %>%
    rev() %>%
    .[1:24] %>%
    rev()


  trend <- decomp %>%
    pull(trend)

  last_n_obs <- rev(trend)[1:.n] %>% rev()

  t <- 1:.n

  y1 <- exp(.ff * t)

  wm <- diff_vec(last_n_obs) %>%
    weighted.mean(na.rm = TRUE, w = y1, trim = 0.4)

  y2 <- exp(-.dec * 1:.l)

  ff_diff <- seq(wm, 0, length.out = .l)

  ff_diff2 <- rep(wm, .l) * y2

  modelled_trend <- c(cumsum(ff_diff2) + rev(last_n_obs)[1])

  result <- tibble(
    date = future_dttm,
    durchfluss = seasonal_carried + modelled_trend,
    type = "forec"
  )

  rlang::inform("forecast created")

  return(result)
}

sna

a <- df %>%
  select(date, durchfluss) %>%
  rolling_origin(
    initial = 100,
    assess = 24,
    cumulative = FALSE,
    skip = 24
  )

errs <- vector(length = length(a$splits))

flex_opti <- function(x) {
  err <- vector(length = length(a$splits))
  for (i in seq_along(a$splits)) {
    fore <- flex_forecast(
      df = a$splits[[i]] %>% analysis(),
      .value = durchfluss,
      .seasonal_period = 24,
      .seasonal_window = x[1],
      .trend_window = x[2],
      .l = 24,
      .dec = x[3],
      .ff = x[4],
      .n = x[5]
    ) %>% pull(durchfluss)

    cons <- a$splits[[i]] %>%
      assessment() %>%
      pull(durchfluss)

    err[i] <- sum(abs(fore - cons))
  }

  return(sum(err))
}
#
#
# optim(
#   par = c(1, 1, 0.3, 0.2, 10), flex_opti
# )
#
