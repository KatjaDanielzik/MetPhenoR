#' Plot Final Time Point per Experimental Condition
#'
#' This function extracts the final (latest) time point for each well across
#' experimental conditions and visualizes the corresponding measurements in a
#' 96-well plate layout.
#'
#' Each plot represents a single experimental condition and displays wells as
#' spatially arranged points, colored according to their measured value at the
#' final time point.
#'
#' @param data A named list produced by \code{read_folder()}, containing:
#' \describe{
#'   \item{data}{A \code{data.frame} with experimental measurements}
#'   \item{meta}{A list containing metadata, including a character vector
#'   \code{variables} specifying grouping variables}
#' }
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Groups the data by experimental variables, plate type, and well.
#'   \item Extracts the latest time point per well.
#'   \item Converts well identifiers (e.g., "A01") into plate coordinates.
#'   \item Generates a separate plate-layout plot for each experimental condition.
#' }
#'
#' Color intensity reflects the magnitude of \code{diff_OD_590_740} at the final
#' recorded time point.
#'
#' @return A named list of \code{ggplot2} objects, where each element corresponds
#'   to a unique experimental condition.
#'
#' @import dplyr
#' @import ggplot2
#' @import magrittr
#' @importFrom tidyr unite
#' @importFrom stringr str_extract
#' @importFrom purrr map_chr map
#' @importFrom tidyselect all_of
#'
#'@examples
#' \dontrun{
#' plots <- plot_latest_timepoint(data)}
#' 
#' 
#' @export

.plot_latest_point <- function(df) {
  
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


#' Classify Growth Curve Dynamics
#'
#' Classifies growth curve dynamics from optical density (OD) time-series data
#' using smoothing splines and their first derivatives. Curve behavior is inferred
#' based on derivative thresholds and quality control (QC) metrics, enabling
#' categorization into biologically interpretable growth states.
#'
#' The classification distinguishes between actively growing, declining,
#' flat (low signal), slow-growing, and uncertain curves. Uncertainty is assigned
#' when data quality is insufficient or exhibits excessive noise.
#'
#' @param data A structured list containing:
#'   \describe{
#'     \item{data}{A data frame of OD measurements in long format.}
#'     \item{meta}{A list containing metadata, including a character vector
#'     \code{variables} defining grouping columns (e.g., strain, condition).}
#'   }
#' @param spar Numeric. Smoothing parameter passed to \code{\link[stats]{smooth.spline}}.
#'   Controls the degree of smoothing (default: \code{0.7}).
#' @param drop_threshold Numeric. Threshold for detecting negative growth phases
#'   based on the first derivative (default: \code{-0.01}).
#' @param tol Numeric. Tolerance around zero for classifying flat growth
#'   (default: \code{0.01}).
#' @param max_od_threshold Numeric. Maximum OD value below which a curve is
#'   classified as low, flat signal (default: \code{0.1}).
#'
#' @details
#' For each well, a smoothing spline is fitted to OD measurements over time.
#' The first derivative of the fitted spline is used to quantify growth dynamics:
#' \itemize{
#'   \item Positive derivatives indicate growth phases.
#'   \item Negative derivatives indicate declines in OD.
#'   \item Near-zero derivatives indicate stationary or flat behavior.
#' }
#'
#' Quality control flags are applied to identify unreliable classifications:
#' \itemize{
#'   \item Low data density (fewer than four time points).
#'   \item High noise, defined as a large standard deviation of derivatives
#'   relative to the OD range.
#' }
#'
#' Curve classification is assigned according to the following hierarchy:
#' \enumerate{
#'   \item \code{"uncertain"}: QC criteria not satisfied.
#'   \item \code{"OD_drop"}: Significant negative derivative detected.
#'   \item \code{"flat_low"}: Flat curve with low maximum OD.
#'   \item \code{"growing"}: Positive growth phase detected.
#'   \item \code{"slow_growing"}: Residual category for weak or inconsistent growth.
#' }
#'
#' @return A list identical in structure to the input, augmented with a
#' \code{classification} element containing:
#' \describe{
#'   \item{derivative_data}{A data frame including spline-fitted values and
#'   first derivatives for each observation.}
#'   \item{well_summary}{A summarized data frame with per-well QC metrics
#'   and assigned curve classifications.}
#' }
#'
#' @import dplyr
#' @importFrom stats smooth.spline predict sd
#'
#' @examples
#' \dontrun{
#' data <- classify_growth_curve_types(data)
#' head(data$classification)
#' }
#'
#' @export
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

#' Estimate Area Under the Curve (AUC) with Bootstrap Confidence Intervals
#'
#' Computes the Area Under the Curve (AUC) for time-series optical density (OD)
#' measurements on a per-well basis using spline-based regression, followed by
#' residual bootstrap to quantify uncertainty.
#'
#' @param data A structured list containing:
#'   \describe{
#'     \item{data}{A data frame of raw OD time-series measurements.}
#'     \item{meta}{Metadata including grouping variables.}
#'     \item{classification}{Well-level annotations (must include
#'     \code{well_summary}).}
#'   }
#' @param curve_type Character string specifying which curve type to include
#'   (default: \code{"growing"}).
#' @param n_boot Integer. Number of bootstrap replicates (default: 1000).
#' @param spline_df Integer. Degrees of freedom for natural spline fitting
#'   (default: 4).
#' @param seed Integer. Random seed for reproducibility (default: 123).
#' @param store Logical. If \code{TRUE}, results are stored in
#'   \code{data$auc_results}.
#'
#' @return A tibble or modified data object (see details).
#'
#' @import dplyr
#' @importFrom stats lm fitted residuals quantile
#' @importFrom splines ns
#'
#' @examples
#' \dontrun{
#' data <- compute_auc_ci(data)
#' head(data$auc_results)
#' }
#'
auc_trapz <- function(x, y) {
  idx <- order(x)
  x <- x[idx]
  y <- y[idx]
  sum(diff(x) * (head(y, -1) + tail(y, -1)) / 2)
}
