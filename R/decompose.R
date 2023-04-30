#' @title decompose msts
#' @param df a dataframe
#' @param .date_var the date variable
#' @param .value a target variable
#' @param .seasonal_periods a single or vector of seasonal periods
#' @param t.window trend window
#' @param t.degree degree of polynomial for fitting trend
#' @export

decompose_msts <- function(df, .date_var, .value, .seasonal_periods = 24, t.window = NULL, t.degree = 1) {

    msts_data <- df %>%
        arrange({{.date_var}}) %>%
        pull({{.value}}) %>%
        forecast::msts(
            seasonal.periods = .seasonal_periods
        )

    df <- bind_cols(
        df,
        msts_data %>%
            forecast::mstl(
                x = .,
                t.window = t.window,
                t.degree = t.degree
            ) %>%
            as_tibble() %>%
            janitor::clean_names() %>%
            select(-data)
    )

    return(df)
}


#' @title forecast msts
#' @param df a dataframe
#' @param .date_var date variable
#' @param .value target variable
#' @param .seasonal_periods seasonal periods
#' @param .trend_window a trend window
#' @param .length_out forecast horizon
#' 

forecast_msts <- function(df, .date_var, .value, .seasonal_periods, .trend_window, .seasonal_window, .length_out, .method = "naive") {

    msts_data <- df %>%
      pull({{.value}}) %>%
      forecast::msts(
        seasonal.periods = .seasonal_periods
      )

    msts_forecast <- msts_data %>%
      forecast::stlf(
        h = .length_out, 
        #t.window = .trend_window, 
        #s.window = .seasonal_window, 
        method = .method, 
        lambda = "auto"
        ) %>%
      forecast::forecast()

    recomb_df <- df %>%
      timetk::future_frame(
        {{.date_var}}, .length_out = .length_out
      ) %>%
      bind_cols(as_tibble(msts_forecast)) %>%
      janitor::clean_names() %>%
      dplyr::select({{.date_var}}, point_forecast)

      return(recomb_df)
} 