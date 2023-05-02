devtools::load_all()


library(lubridate)
library(readr)
library(changepoint)
library(forecast)
library(tidyverse)
library(timetk)
library(foreach)

out <- extract_series(.building_id = 25, q = 5, last = 2)

first_pacf <- out %>%
  tk_acf_diagnostics(.date_var = timestamp, meter_reading, .lags = 200) %>%
  mutate(abs_ACF = abs(ACF)) %>%
  filter(lag > 5) %>%
  arrange(desc(abs_ACF)) %>%
  mutate(diff = lag(lag) - lag) %>%
  filter(abs(diff) > 5)

vec <- c(first_pacf$lag[1:5])
vec <- c(168, 24)

hist_ext_tbl <- out %>%
  mutate(
    meter_reading = smooth_vec(meter_reading, period = 4),
    meter_reading = ts_clean_vec(meter_reading, period = 24)
         ) %>%
  select(timestamp, meter_reading) %>%
  add_components(.value = meter_reading, .seasonal_periods = vec, .trend_window = nrow(.)/3, .iterate = 100) %>%
  mutate(type = "hist") %>%
  future_frame(.date_var = timestamp, .length_out = 24*3, .bind_data = TRUE)

seasonal_components <- hist_ext_tbl %>%
  colnames() %>%
  str_detect("seasonal") %>%
  colnames(hist_ext_tbl)[.]

seasonal_periods <- parse_number(seasonal_components)

foreach::foreach(i = seasonal_components, j = seasonal_periods) %do% {
  hist_ext_tbl[[i]] <- seas_lag(hist_ext_tbl[[i]], n = j)
}

filled_tbl <- hist_ext_tbl %>% (
  function(x, var = trend) {

    var <- enquo(var)

    trend_component <- x %>%
      pull(!!var) %>%
      na.omit()

  fore  <- trend_component %>% ts(frequency = 24) %>%
      naive(h = 24*3) %>%
    as_tibble() %>%
    janitor::clean_names() %>%
    .[['point_forecast']]


  fore_comb <- c(trend_component, fore)

  out <- x %>% mutate(trend_fore = fore_comb)

  return(out)
  }
)

filled_tbl %>% (
  function(x){
    vars <- syms(seasonal_components)
    x %>%
      rowwise() %>%
      mutate(forecast = sum(!!!vars, trend_fore))
  }
) %>%
  select(timestamp, where(is.numeric)) %>%
  pivot_longer(-timestamp) %>%
  plot_time_series(
    timestamp, value, name, .smooth =FALSE
  )




