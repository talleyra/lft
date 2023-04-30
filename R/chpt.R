#' @title add changepoints to timeseries
#' @param df a dataframe
#' @param .value a target variable
#' @param fun either mean or var
#' @param q number of changepoints
#' @export

add_changepoints <- function(df, .value, fun = "mean", q = NULL, name = "", last = NULL) {

  var_vec <- df %>% pull({{.value}}) %>% ts()

  cpt.fun <- switch(
    fun,
    "mean" = cpt.mean,
    "var" = cpt.var
  )

  algo <- switch(
    fun,
    "mean" = "BinSeg",
    "var"  = "BinSeg"
  )

  points <- cpt.fun(var_vec, method = algo, Q = q)

  points_vec <- c(1, cpts(points))

  cpt_tbl <- tibble(nr = points_vec) %>%
    mutate("cp_{fun}_{name}" := str_c(glue::glue("cp_{fun}_"), nr))

  df_out <- df %>%
    mutate(nr = row_number()) %>% 
    left_join(cpt_tbl, by = "nr") %>%
    dplyr::select(- nr) %>%
    fill(glue::glue("cp_{fun}_{name}"), .direction = "down")


  if(!is.null(last)) {

    chpts_filter <- cpt_tbl %>%
      slice_tail(n = last) %>%
      pull(glue::glue("cp_{fun}_{name}"))

      print(chpts_filter)

    df_out <- df_out %>%
      filter(!!sym(glue::glue("cp_{fun}_{name}")) %in% chpts_filter)
  }

  return(df_out)

}
