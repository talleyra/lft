#' @title add components from stl
#' @param df a dataframe
#' @param .value a target variable
#' @param .seasonal_periods seasonal periods or just a frequency
#' @export

add_components <- function(df, .value, .seasonal_periods, .trend_window = NULL, .seasonal_window = 7 + 4 * seq(6), .iterate = 2) {
  dec <- df %>%
    pull({{ .value }}) %>%
    ts() %>%
    tsclean() %>%
    msts(seasonal.periods = .seasonal_periods) %>%
    mstl(t.window = .trend_window, s.window = .seasonal_window, iterate = .iterate)

  adj <- seasadj(dec)

  dec_tbl <- dec %>%
    as_tibble() %>%
    janitor::clean_names() %>%
    select(-data)

  df_out <- df %>%
    bind_cols(dec_tbl) %>%
    mutate(seasadj = adj)

  return(df_out)
}
