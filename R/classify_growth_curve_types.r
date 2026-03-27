#' Classify Growth Curve Types Using Spline Derivatives
#'
#' Fits smoothing splines to time-course data per well and classifies
#' growth behavior based on first derivatives.
#'
#' @param data A data frame containing time-course measurements.
#' @param variables Character vector of experimental variables.
#' @param spar Smoothing parameter for spline fitting.
#' @param drop_threshold Threshold for detecting decreasing trends.
#' @param tol Tolerance for flat behavior.
#' @param max_od_threshold Threshold for low OD flat classification.
#'
#' @return A list with:
#' \describe{
#'   \item{derivative_data}{Data with spline fits and derivatives}
#'   \item{well_summary}{Per-well classification summary}
#' }
#' @import magrittr
#' @import dplyr
#' @importFrom stats smooth.spline
#' @importFrom stats predict
#' @importFrom stats sd
#' 
#' 
#' @export
classify_growth_curve_types <- function(
    data,
    variables,
    spar = 0.7,
    drop_threshold = -0.01,
    tol = 0.01,
    max_od_threshold = 0.1
) {
  
  # ---- Input validation ----
  required_cols <- c(variables, "Plate.Type", "Well", "Hour", "diff_OD_590_740")
  missing_cols <- setdiff(required_cols, names(data))
  
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # ---- Ensure proper ordering ----
  data <- dplyr::arrange(
    data,
    dplyr::across(dplyr::all_of(c(variables, "Plate.Type", "Well", "Hour")))
  )
  
  # ---- Safe spline fitting function (NEW) ----
  .fit_spline_safe <- function(df) {
    
    # Handle insufficient data
    if (nrow(df) < 4 || all(is.na(df$diff_OD_590_740))) {
      return(dplyr::mutate(
        df,
        spline = NA_real_,
        first_derivative = NA_real_
      ))
    }
    
    fit <- tryCatch(
      stats::smooth.spline(x = df$Hour, y = df$diff_OD_590_740, spar = spar),
      error = function(e) NULL
    )
    
    if (is.null(fit)) {
      return(dplyr::mutate(
        df,
        spline = NA_real_,
        first_derivative = NA_real_
      ))
    }
    
    # global binding of variables
    n_points <- NULL
    
    dplyr::mutate(
      df,
      spline = stats::predict(fit, x = df$Hour)$y,
      first_derivative = stats::predict(fit, x = df$Hour, deriv = 1)$y
    )
  }
  
  # ---- Step 1: Fit spline + derivative ----
  deriv_data <- data %>%
    dplyr::group_by(
      dplyr::across(dplyr::all_of(c(variables, "Plate.Type", "Well")))
    ) %>%
    dplyr::group_modify(~ .fit_spline_safe(.x)) %>%
    dplyr::ungroup()
  
  # ---- Step 2: Well-level classification ----
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
      
      # ---- NEW QC METRICS ----
      derivative_sd = stats::sd(.data$first_derivative, na.rm = TRUE),
      od_range = diff(range(.data$diff_OD_590_740, na.rm = TRUE)),
      
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      qc_low_points = .data$n_points < 4,
      qc_high_noise = .data$derivative_sd > (0.1 * .data$od_range),
      qc_flag = .data$qc_low_points | .data$qc_high_noise,
      
      curve_type = dplyr::case_when(
        .data$qc_flag ~ "uncertain",
        .data$has_drop ~ "OD_drop",
        .data$all_flat & .data$max_od <= max_od_threshold ~ "flat_low",
        .data$has_growth ~ "growing",
        TRUE ~ "slow_growing"
      )
    )
      
  return(list(
    derivative_data = deriv_data,
    well_summary = well_summary
  ))
}