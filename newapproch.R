
library(tidyverse)


out %>%
  pull(meter_reading) %>%
  ts() %>%
  tsclean() %>%
  autoplot()


p <- out %>%
  pull(meter_reading) %>%
  ts() %>%
  pacf(plot = FALSE, )


df_decomp <- out %>%
  add_components(.value = meter_reading,.seasonal_periods = 166, .trend_window = 1000, .seasonal_window = 100)

df_seas_carried <- df_decomp %>%
  timetk::future_frame(timestamp, .length_out = 400, .bind_data = TRUE) %>%
  mutate(seasonal166 = seas_lag(seasonal166, n = 166))


trend <- df_decomp %>% pull(trend) %>%
  ts() %>% ses(h = 400)


trend_vec <- c(
  df_decomp %>% pull(trend),
  trend %>% as_tibble() %>%
  janitor::clean_names() %>%
  pull(point_forecast)
)


df_seas_carried %>%
  mutate(trend = trend_vec) %>%
  mutate(forecast = trend + seasonal166) %>%
  select(timestamp, where(is.numeric)) %>%
  select(timestamp, trend, forecast, meter_reading,seasonal166, remainder, air_temperature) %>%
  add
  timetk::tk_acf_diagnostics(timestamp, remainder) %>%
  view()
  #select(where(is.numeric), -c(forecast, meter_reading)) %>%
  #lm(remainder ~ ., data = .) %>%
  #summary()
  #corrr::correlate()
  #mutate(across(where(is.numeric), timetk::standardize_vec)) %>%
  pivot_longer(-timestamp) %>%
  timetk::plot_time_series(
    timestamp, value, name, .smooth = FALSE
  )


### idea model the amplitude of the load curve

