devtools::load_all()


library(lubridate)
library(readr)
library(changepoint)
library(forecast)
library(tidyverse)
library(timetk)
library(foreach)

out <- extract_series(.building_id = 96, q = 5, last = 1)



first_pacf <- out %>%
  tk_acf_diagnostics(.date_var = timestamp, meter_reading, .lags = 200) %>%
  mutate(abs_ACF = abs(ACF)) %>%
  filter(lag > 23) %>%
  #filter(lag %in% c(24, 24*7)) %>%
  arrange(desc(abs_ACF))

sp1 <- first_pacf$lag[1]
print(sp1)
out %>%
  add_components(.value = meter_reading, .seasonal_periods = sp1, .iterate = 200, .seasonal_window = 5) %>%
  select(timestamp, meter_reading, seasonal168, asp1 = seasadj) %>%
  add_components(.value = asp1, .seasonal_periods = 24, .iterate = 200) %>%
  mutate(ff = trend + seasonal168 + seasonal24) %>%
  pivot_longer(-timestamp) %>%
  plot_time_series(timestamp, value, name, .smooth = FALSE)
  #tk_acf_diagnostics(.date_var = timestamp, seasadj, .lags = 200) %>%
  mutate(abs_ACF = abs(ACF)) %>%
  filter(lag > 23) %>%
  arrange(desc(abs_ACF))

