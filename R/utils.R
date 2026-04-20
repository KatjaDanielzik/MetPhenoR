#' Plot Latest Measurement Point
#'
#' Generates a plate-style scatter plot representing the final time point
#' measurements from a given data frame. Each well position is visualized
#' as a point, with color intensity corresponding to the difference in
#' optical density (OD 590–740 nm).
#'
#' @param df A data frame containing at least the following columns:
#' \describe{
#'   \item{number}{Numeric or integer indicating column position (e.g., 1–12).}
#'   \item{category}{Factor or character indicating row position (e.g., A–H).}
#'   \item{diff_OD_590_740}{Numeric values representing OD differences used for coloring.}
#'   \item{Plate Type}{Character or factor indicating the plate type.}}
#'   
#'  @param variables specifying grouping variables
#'
#' @returns A \code{\link[ggplot2]{ggplot}} object showing the plate layout
#' with color-coded measurement values.
#' 
#' @keywords internal
#' 
#' @importFrom ggplot2 ggplot aes geom_point scale_x_continuous
#'   scale_y_discrete scale_color_gradient coord_fixed labs theme_minimal
#' @importFrom purrr map_chr
#' @importFrom scales squish
#' @importFrom rlang .data
.plot_latest_point <- function(df,variables) {
  
  # Construct descriptive subtitle
  var_values <- purrr::map_chr(variables, function(v) {
    paste0(v, ": ", df[[v]][1])
  })
  
  subtitle_text <- paste(
    paste(var_values, collapse = " | "),
    paste("Plate:", unique(df$"Plate Type")),
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
      title = "Measurement at Final Time Point",
      subtitle = subtitle_text,
      x = NULL,
      y = NULL,
      color = "diff_OD_590_740"
    ) +
    ggplot2::theme_minimal(base_size = 14)
}


#' Fit Smoothing Spline
#'
#' @param df_group A data frame representing a single group of observations.
#' Must contain the following columns:
#' \describe{
#'   \item{Hour}{Numeric vector representing the time variable.}
#'   \item{diff_OD_590_740}{Numeric response variable used for spline fitting.}
#' }
#'
#' @return The input data frame with two additional numeric columns:
#' \describe{
#'   \item{spline}{Fitted values from the smoothing spline.}
#'   \item{first_derivative}{First derivative of the fitted spline.}
#' }
#' 
#' @keywords internal
#'
#' @importFrom dplyr mutate
#' @importFrom stats smooth.spline predict
#'
.fit_spline_safe <- function(df_group) {
  
  if (nrow(df_group) < 4 || all(is.na(df_group$diff_OD_590_740))) {
    return(dplyr::mutate(
      df_group,
      spline = NA_real_,
      first_derivative = NA_real_
    ))
  }
  
  fit <- tryCatch(
    stats::smooth.spline(x = df_group$Hour, y = df_group$diff_OD_590_740, spar = spar),
    error = function(e) NULL
  )
  
  if (is.null(fit)) {
    return(dplyr::mutate(
      df_group,
      spline = NA_real_,
      first_derivative = NA_real_
    ))
  }
  
  dplyr::mutate(
    df_group,
    spline = stats::predict(fit, x = df_group$Hour)$y,
    first_derivative = stats::predict(fit, x = df_group$Hour, deriv = 1)$y
  )
}

