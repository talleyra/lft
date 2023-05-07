devtools::load_all()


library(lubridate)
library(readr)
library(changepoint)
library(forecast)
library(tidyverse)
library(timetk)
library(foreach)
library(openmeteoapi)

rain <- openmeteoapi::om_get_data(
  latitude = 46.67,
  longitude = 11.16,
  variables = "rain",
  start_date = min(df$date),
  end_date = max(df$date)
)


### get a dataset
df <- read_csv("passerdata.csv") #%>%
  #select(date, durchfluss) %>%
  slice_head(prop = 1/4)

.sp = 24
.l = 100

# add components
decomp <- df %>%
  add_components(.seasonal_periods = .sp, durchfluss, .seasonal_window = 3, .trend_window = 24*2)

x = 3

get_error_rain <- function(x){

error <- decomp %>%
  #mutate(lufttemperatur = standardize_vec(slidify_vec(lufttemperatur, mean, .period = 24))) %>%
  mutate(ll = lag(niederschlag, as.integer(x[1]))*x[2]) %>%
  mutate(ll = slidify_vec(ll, mean, .period = 5)) %>%
  mutate(diff = durchfluss - trend) %>%
  mutate(err = diff - ll) %>%
  pull(err) %>%
  abs() %>%
  sum(., na.rm = TRUE)

return(error)
}


error <- decomp %>%
  left_join(rain, by = c("date" = "time")) %>%
  mutate(lufttemperatur = standardize_vec(slidify_vec(lufttemperatur, mean, .period = 24))) %>%
  mutate(ll = lag(niederschlag, as.integer(7))*10) %>%
  mutate(ll = slidify_vec(ll, mean, .period = 5)) %>%
  mutate(diff = durchfluss - trend) %>%
  mutate(err = diff - ll)

error %>%
  select(date, ll, diff, err, lufttemperatur, rain, niederschlag) %>%
  fill(rain) %>%
  mutate(rr = TTR::EMA(rain, n = 4)) %>%
  pivot_longer(-date) %>%
  plot_time_series(
    date, value, name, .smooth = FALSE
  )

optim(par = c(1,1), get_error_rain)


decomp

# extract future dateseries
future_dttm = decomp %>%
  timetk::tk_index() %>%
  timetk::tk_make_future_timeseries(
    length_out = .l
  )

# carry the seasonal component

seasonal_carried = decomp %>%
  slice_tail(n = .sp) %>%
  pull(seasonal24) %>%
  rev() %>%
  .[1:24] %>%
  rev()

# get n last trend components

n = 24

trend <- decomp %>%
  pull(trend)

last_n_obs <- rev(trend)[1:n] %>% rev()

t = 1:n

y1 = exp(.ff * t)

wm <- timetk::diff_vec(last_n_obs) %>%
  weighted.mean(na.rm = TRUE, w = y1, trim = 0.4)

y2 = exp(- .dec * 1:.l)

ff_diff <- seq(wm, 0, length.out = .l)

ff_diff2 <- rep(wm, .l) * y2

modelled_trend = c(cumsum(ff_diff2) + rev(last_n_obs)[1])

# put all together

seasonal_carried + modelled_trend

tibble(
  date = future_dttm,
  durchfluss = seasonal_carried + modelled_trend,
  type = "forec"
) %>%
  bind_rows(df %>% mutate(type = "hist")) %>%
  plot_time_series(date, durchfluss, type, .smooth = FALSE)



