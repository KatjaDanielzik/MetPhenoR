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
#' @importFrom rlang .data
#'
#' @examples
#' data("processed_data")
#' processed_data <- classify_growth_curve_types(processed_data)
#' head(processed_data$classification)
#'
#' @export
classify_growth_curve_types <- function(
  data,
  spar = 0.7,
  drop_threshold = -0.01,
  tol = 0.01,
  max_od_threshold = 0.1
) {
  # ---- Validate top-level data structure ----
  if (!is.list(data) || !all(c("data", "meta") %in% names(data))) {
    stop("'data' must be a named list with elements 'data' and 'meta',
          as produced by read_folder().")
  }

  if (!is.list(data$meta) || !"variables" %in% names(data$meta)) {
    stop("'data$meta' must be a named list containing 'variables',
          as produced by read_folder().")
  }

  # ---- Extract ----
  df <- data$data
  variables <- data$meta$variables

  # ---- Validate data$data ----
  if (!is.data.frame(df)) {
    stop("'data$data' must be a data.frame.")
  }

  if (nrow(df) == 0) {
    stop("'data$data' must contain at least one row.")
  }

  # ---- Validate variables ----
  if (!is.character(variables) || length(variables) == 0) {
    stop("'data$meta$variables' must be a non-empty character vector.")
  }

  # ---- Validate required columns ----
  required_cols <- c(variables, "Plate.Type", "Well", "Hour", "diff_OD_590_740")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }

  # ---- Validate numeric parameters ----
  if (!is.numeric(spar) || length(spar) != 1 || spar < 0 || spar > 1) {
    stop("'spar' must be a single numeric value between 0 and 1.")
  }

  if (!is.numeric(drop_threshold) || length(drop_threshold) != 1) {
    stop("'drop_threshold' must be a single numeric value.")
  }

  if (!is.numeric(tol) || length(tol) != 1 || tol < 0) {
    stop("'tol' must be a single non-negative numeric value.")
  }

  if (!is.numeric(max_od_threshold) || length(max_od_threshold) != 1 ||
    max_od_threshold < 0) {
    stop("'max_od_threshold' must be a single non-negative numeric value.")
  }

  # Data read in
  variable <- NULL
  df <- data$data
  variables <- data$meta$variables

  # Arrange
  df <- dplyr::arrange(
    df,
    dplyr::across(dplyr::all_of(c(variables, "Plate.Type", "Well", "Hour")))
  )

  # Derivatives
  deriv_data <- df %>%
    dplyr::group_by(
      dplyr::across(dplyr::all_of(c(variables, "Plate.Type", "Well")))
    ) %>%
    dplyr::group_modify(~ .fit_spline_safe(.x, spar)) %>%
    dplyr::ungroup()

  # Summary
  well_summary <- deriv_data %>%
    dplyr::group_by(
      dplyr::across(dplyr::all_of(c(variables, "Plate.Type", "Well")))
    ) %>%
    dplyr::summarise(
      n_points = dplyr::n(),
      has_drop = any(.data$first_derivative < drop_threshold, na.rm = TRUE),
      has_growth = any(.data$first_derivative > tol, na.rm = TRUE),
      all_flat = all(abs(.data$first_derivative) <= tol, na.rm = TRUE),
      max_od = max(.data$diff_OD_590_740, na.rm = TRUE),
      derivative_sd = stats::sd(.data$first_derivative, na.rm = TRUE),
      od_range = diff(range(.data$diff_OD_590_740, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      qc_low_points = .data$n_points < 4,
      qc_high_noise = .data$derivative_sd > (0.1 * .data$od_range),
      qc_flag = .data$qc_low_points | .data$qc_high_noise,
      curve_type = dplyr::case_when(
        qc_flag ~ "uncertain",
        has_drop ~ "OD_drop",
        all_flat & max_od <= max_od_threshold ~ "flat_low",
        has_growth ~ "growing",
        TRUE ~ "slow_growing"
      )
    )

  # Attach results back
  data$classification <- list(
    derivative_data = deriv_data,
    well_summary = well_summary
  )

  return(data)
}
