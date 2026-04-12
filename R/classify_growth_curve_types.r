#' Classify Growth Curves 
#'
#' Classifies growth curve behavior based on smoothed splines and first derivatives
#' of optical density (OD) measurements. The function assigns curve types such as
#' growing, flat, OD_drop, or uncertain based on derivative thresholds and QC metrics.
#'
#' @param data A list containing:
#'   \itemize{
#'     \item \code{data} A data frame of measurements
#'     \item \code{meta} A list containing metadata, including \code{variables}
#'   }
#' @param spar Numeric smoothing parameter passed to \code{stats::smooth.spline} (default: 0.7)
#' @param drop_threshold Numeric threshold for detecting negative growth drops in first derivative (default: -0.01)
#' @param tol Numeric tolerance for considering growth as flat (default: 0.01)
#' @param max_od_threshold Numeric maximum OD threshold used to define flat low curves (default: 0.1)
#'
#' @return A list with the same structure as input, with an additional \code{classification}
#' element containing:
#' \itemize{
#'   \item \code{derivative_data} Data frame with spline and first derivative values
#'   \item \code{well_summary} Summary table with curve classification per well
#' }
#'
#' @import dplyr
#' @importFrom stats smooth.spline predict sd
#'
#' @export

classify_growth_curve_types <- function(
    data,
    spar = 0.7,
    drop_threshold = -0.01,
    tol = 0.01,
    max_od_threshold = 0.1
) {
  
  # ---- Validate structure ----
  if (!is.list(data) || !"data" %in% names(data)) {
    stop("Input must be a list with a 'data' element")
  }
  
  if (!"meta" %in% names(data) ||
      !"variables" %in% names(data$meta)) {
    stop("Missing 'meta$variables' in data")
  }
  
  df <- data$data
  variables <- data$meta$variables
  
  # ---- Column validation ----
  required_cols <- c(variables, "Plate Type", "Well", "Hour", "diff_OD_590_740")
  missing_cols <- setdiff(required_cols, names(df))
  
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # ---- Arrange ----
  df <- dplyr::arrange(
    df,
    dplyr::across(dplyr::all_of(c(variables, "Plate Type", "Well", "Hour")))
  )
  
  # ---- Safe spline ----
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
  
  # ---- Derivatives ----
  deriv_data <- df %>%
    dplyr::group_by(
      dplyr::across(dplyr::all_of(c(variables, "Plate Type", "Well")))
    ) %>%
    dplyr::group_modify(~ .fit_spline_safe(.x)) %>%
    dplyr::ungroup()
  
  # ---- Summary ----
  well_summary <- deriv_data %>%
    dplyr::group_by(
      dplyr::across(dplyr::all_of(c(variables, "Plate Type", "Well")))
    ) %>%
    dplyr::summarise(
      n_points = dplyr::n(),
      has_drop = any(first_derivative < drop_threshold, na.rm = TRUE),
      has_growth = any(first_derivative > tol, na.rm = TRUE),
      all_flat = all(abs(first_derivative) <= tol, na.rm = TRUE),
      max_od = max(diff_OD_590_740, na.rm = TRUE),
      derivative_sd = stats::sd(first_derivative, na.rm = TRUE),
      od_range = diff(range(diff_OD_590_740, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      qc_low_points = n_points < 4,
      qc_high_noise = derivative_sd > (0.1 * od_range),
      qc_flag = qc_low_points | qc_high_noise,
      curve_type = dplyr::case_when(
        qc_flag ~ "uncertain",
        has_drop ~ "OD_drop",
        all_flat & max_od <= max_od_threshold ~ "flat_low",
        has_growth ~ "growing",
        TRUE ~ "slow_growing"
      )
    )
  
  # ---- Attach results back ----
  data$classification$derivative_data <- deriv_data
  data$classification$well_summary <- well_summary
  
  return(data)
}
