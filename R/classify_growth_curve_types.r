#' Classify Growth Curves in Processed Data Object
#'
#' @param data A list containing:
#'   - data: data frame
#'   - meta: list with variables
#'   
#'   @import dplyr
#'   @importFrom stats smooth.spline
#'   @importFrom stats predict
#'   @importFrom stats sd
#'   
#'
#' @return Updated data list with results appended
#' @export

classify_growth_curve_types <- function(
    data,
    variables = data$meta$variables,
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
