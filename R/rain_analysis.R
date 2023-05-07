


df %>%
  fill(niederschlag) %>%
  mutate(baseline = durchfluss - slidify_vec(lag(durchfluss, 24), .period = 24*2, median, .align = "right")) %>%
  mutate(nn = slidify_vec(lag(niederschlag, 2.5), mean, .period = 10) * 25) %>%
  fill(nn) %>%
  mutate(nne = TTR::EMA(nn, ratio = 0.1)) %>%
  mutate(fff = nne + slidify_vec(.f = median, .x = durchfluss, .period = 12, .align = "right")) %>%
  #.[rain_rain_after, ] %>%
  pivot_longer(-date) %>%
  timetk::plot_time_series(
    date, value, name, .smooth = FALSE, .smooth_period = FALSE
  )
  timetk::plot_acf_diagnostics(
    .date_var = date, durchfluss, .ccf_vars = nn,
  )


rain_events = which(df$niederschlag > 0.1)

li = list()
for(i in seq_along(rain_events)){
  li[[i]] = seq(rain_events[i], rain_events[i] + 24)
}

rain_rain_after <- li %>%
  unlist() %>%
  unique()


df[rain_rain_after, ] %>%
  mutate(nr = row_number()) %>%
  select(nr, durchfluss, niederschlag) %>%
  pivot_longer(-nr) %>%
  ggplot(aes(nr, value, color = name, group = name)) +
  geom_line()
  view()

