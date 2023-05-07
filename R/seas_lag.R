#' @title function for filling missing seasonality
#' @param values a vecotor of seasonalities
#' @param n the period of the seasonality
#' @export

seas_lag <- function(values, n) {
  for (i in seq_along(1:length(values))) {
    if (is.na(values[i])) {
      #    print(values[i])
      values[i] <- lag(values, n)[i]
    }
  }

  return(values)
}
