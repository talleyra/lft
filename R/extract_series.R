

extract_series <- function(.building_id, q, last = NULL){

  df <- read_csv("train.csv")

  df_red <- df %>%
    dplyr::filter(building_id == .building_id)

  df_red_padded <- df_red %>%
    timetk::pad_by_time(.date_var = timestamp, .by = "hour", .fill_na_direction = "down") %>%
    arrange(timestamp) %>%
    add_changepoints(.value = meter_reading, q = q, last = last) %>%
    dplyr::select(timestamp, meter_reading, cp_mean, where(is.numeric))


  return(df_red_padded)

}
