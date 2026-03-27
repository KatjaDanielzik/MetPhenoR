# ---- Safe spline fitting function (NEW) ----
#' Title
#'
#' @param df
#'
#' @returns 
#' @keywords internal

.fit_spline_safe <- function(df) {
  
  # Handle insufficient data
  if (nrow(df) < 4 || all(is.na(df$raw_value))) {
    return(dplyr::mutate(
      df,
      spline = NA_real_,
      first_derivative = NA_real_
    ))
  }
}