# ---- Plot function (plot_latest_timepoint) ----
#' Plot the latest time point in a 96-well plate layout
#' 
#' Generates a plate-style scatter plot representing optical density (OD)
#' values at the latest recorded time point. Each well is displayed as a point
#' positioned according to its plate coordinates and colored by OD difference.
#' 
#' @param df A data frame containing plate data. Must include the following columns:
#' \describe{
#'   \item{number}{Numeric column indicating well column position (1–12).}
#'   \item{category}{Factor or character indicating well row (e.g., A–H).}
#'   \item{diff_OD_590_740}{Numeric values used for color mapping.}
#'   \item{Plate.Type}{Plate identifier used in subtitle annotation.}
#' }
#' @return A \code{ggplot2} object representing the 96-well plate.
#'
#' @keywords internal
#' @import ggplot2
#' @importFrom purrr map_chr
#' @importFrom scales squish
#' 
#' 
.plot_latest_point <- function(df, variables) {
  
  var_values <- purrr::map_chr(variables, function(v) {
    paste0(v, ": ", df[[v]][1])
  })
  
  subtitle_text <- paste(
    paste(var_values, collapse = " | "),
    paste("Plate:", unique(df$Plate.Type)),
    sep = " | "
  )
  
  ggplot2::ggplot(df, ggplot2::aes(x = .data$number, y = .data$category)) +
    ggplot2::geom_point(
      ggplot2::aes(color = .data$diff_OD_590_740),
      size = 8
    ) +
    ggplot2::scale_x_continuous(breaks = 1:12) +
    ggplot2::scale_y_discrete(limits = rev) +
    ggplot2::scale_color_gradient(
      low = "white",
      high = "purple",
      oob = scales::squish
    ) +
    ggplot2::coord_fixed() +
    ggplot2::labs(
      title = "OD at Latest Time Point",
      subtitle = subtitle_text,
      x = NULL,
      y = NULL,
      color = "diff_OD_590_740"
    ) +
    ggplot2::theme_minimal(base_size = 14)
}

# ---- Safe spline fitting function (classify_growth_curve_types) ----
#' Safely fit spline and compute derivatives for growth curves
#'
#' Performs basic quality control before spline fitting. If the input data
#' is insufficient (e.g., too few observations or all missing values),
#' the function returns \code{NA}-filled columns instead of failing.
#'
#' @param df A data frame containing time-series measurements. Must include:
#' \describe{
#'   \item{raw_value}{Numeric vector of observed values used for spline fitting.}
#' }
#'
#' @details
#' A minimum of 4 non-missing observations is required for spline fitting.
#' If this condition is not met, the function returns the original data frame
#' with additional columns:
#' \itemize{
#'   \item \code{spline}: Fitted spline values (NA if fitting fails)
#'   \item \code{first_derivative}: First derivative of the spline (NA if fitting fails)
#' }
#' This function is intended for internal use in growth curve classification pipelines.
#' 
#' @return A data frame with additional columns for spline fit and derivative.
#' @keywords internal
#' @importFrom dplyr mutate

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


