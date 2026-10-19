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
#' @param target_curve_type Character vector specifying which curve types to include
#'   (default: \code{"growing"}).
#' @param n_boot Integer. Number of bootstrap replicates (default: 1000).
#' @param spline_df Integer. Degrees of freedom for natural spline fitting
#'   (default: 4).
#' @param store Logical. If \code{TRUE}, results are stored in
#'   \code{data$auc_results}.
#'
#' @return A tibble or modified data object (see details).
#'
#' @import dplyr
#' @importFrom stats lm fitted residuals quantile
#' @importFrom splines ns
#' @importFrom rlang .data
#'
#' @examples
#' data(processed_data)
#' processed_data <- annotate_chemical(processed_data, annotation = PM_M01_M02)
#' processed_data <- classify_growth_curve_types(processed_data)
#' processed_data <- compute_auc_ci(processed_data)
#' head(processed_data$auc_results)
#'
#' @export
compute_auc_ci <- function(data,
                           target_curve_type = "growing",
                           n_boot = 1000,
                           spline_df = 4,
                           store = TRUE){
  
  # ---- Validate top-level data structure ----
  if (!is.list(data) || !all(c("data", "meta", "classification") %in% names(data))) {
    stop("'data' must be a named list with elements 'data', 'meta', and 'classification',
          as produced by classify_growth_curve_types().")
  }
  
  if (!is.list(data$meta) || !"variables" %in% names(data$meta)) {
    stop("'data$meta' must be a named list containing 'variables',
          as produced by read_folder().")
  }
  
  if (!is.list(data$classification) || !"well_summary" %in% names(data$classification)) {
    stop("'data$classification' must be a named list containing 'well_summary'.
          Run classify_growth_curve_types() first.")
  }
  
  # ---- Extract ----
  raw_data     <- data$data
  variables    <- data$meta$variables
  well_summary <- data$classification$well_summary
  
  # ---- Validate data$data ----
  if (!is.data.frame(raw_data)) {
    stop("'data$data' must be a data.frame.")
  }
  
  if (nrow(raw_data) == 0) {
    stop("'data$data' must contain at least one row.")
  }
  
  # ---- Validate variables ----
  if (!is.character(variables) || length(variables) == 0) {
    stop("'data$meta$variables' must be a non-empty character vector.")
  }
  
  # ---- Validate required columns in data$data ----
  required_cols <- c(variables, "Plate.Type", "Well", "Hour", "diff_OD_590_740")
  missing_cols  <- setdiff(required_cols, names(raw_data))
  if (length(missing_cols) > 0) {
    stop(paste("'data$data' is missing required columns:",
               paste(missing_cols, collapse = ", ")))
  }
  
  # ---- Validate target_curve_type ----
  if (!is.character(target_curve_type) || length(target_curve_type) == 0) {
    stop("'target_curve_type' must be a non-empty character vector.")
  }
  
  valid_curve_types <- c("growing", "OD_drop", "slow_growing", "flat_low")
  unknown_types     <- setdiff(target_curve_type, valid_curve_types)
  if (length(unknown_types) > 0) {
    stop(paste0(
      "'target_curve_type' contains invalid values: ",
      paste(unknown_types, collapse = ", "),
      "\nValid options are: ", paste(valid_curve_types, collapse = ", ")
    ))
  }
  
  # ---- Validate n_boot ----
  if (!is.numeric(n_boot) || length(n_boot) != 1 ||
      is.na(n_boot) || n_boot < 1 || n_boot != round(n_boot)) {
    stop("'n_boot' must be a single positive integer.")
  }
  
  # ---- Validate spline_df ----
  if (!is.numeric(spline_df) || length(spline_df) != 1 ||
      is.na(spline_df) || spline_df < 1 || spline_df != round(spline_df)) {
    stop("'spline_df' must be a single positive integer.")
  }
  
  # ---- Validate store ----
  if (!is.logical(store) || length(store) != 1 || is.na(store)) {
    stop("'store' must be a single logical value (TRUE or FALSE).")
  }
  
  variables <- NULL

  #Join and filter
  combined_df <- dplyr::left_join(
    raw_data,
    well_summary,
    by = c(variables, "Plate.Type", "Well")
  ) %>%
    dplyr::filter(.data$curve_type %in% target_curve_type)
  

  #Global data sufficiency check
  sampling_summary <- combined_df %>%
    dplyr::group_by(.data$PlateId, .data$Well) %>%
    dplyr::summarise(
      n_points = sum(!is.na(.data$diff_OD_590_740)),
      .groups = "drop"
    )
  
  # Get Hour sets per well
  hour_sets <- combined_df %>%
    dplyr::group_by(.data$PlateId, .data$Well) %>%
    dplyr::summarise(
      hour_list = list(sort(unique(.data$Hour))),
      .groups = "drop"
    )
  
  # Compare within each plate
  hour_consistency <- hour_sets %>%
    dplyr::group_by(.data$PlateId) %>%
    dplyr::summarise(
      n_unique_hour_sets = dplyr::n_distinct(.data$hour_list),
      .groups = "drop"
    )
  
  # Warning if inconsistencies exist
  if (any(hour_consistency$n_unique_hour_sets > 1)) {
    warning(
      paste0(
        "Within some plates, wells have inconsistent Hour values. Affected PlateIds: ",
        paste(hour_consistency$PlateId[hour_consistency$n_unique_hour_sets > 1], collapse = ", ")
      )
    )
  }
  
  # Count unique time points per plate
  plate_timepoints <- combined_df %>%
    dplyr::group_by(.data$PlateId) %>%
    dplyr::summarise(
      n_timepoints = dplyr::n_distinct(.data$Hour),
      .groups = "drop"
    )
  
  min_tp <- min(plate_timepoints$n_timepoints)
  max_tp <- max(plate_timepoints$n_timepoints)
  
  if (min_tp != max_tp) {
    warning(
      paste0(
        "Inconsistent number of time points across plates: ",
        "min = ", min_tp,
        ", max = ", max_tp,
        ". This may affect comparability of AUC estimates."
      )
    )
  }
  
  # ---- Per-well estimation ----
  auc_results <- combined_df %>%
    dplyr::group_by(
      dplyr::across(dplyr::all_of(c("Plate.Type", "Well", variables)))
    ) %>%
    dplyr::group_modify(~{
      
      dat <- .x
      
      # ---- Data sufficiency (per well) ----
      n_valid <- sum(!is.na(dat$diff_OD_590_740))
      
      if (n_valid < 4) {
        return(dplyr::tibble(
          AUC = NA_real_,
          AUC_low = NA_real_,
          AUC_high = NA_real_,
          n_points = n_valid,
          Chemical = dat$Chemical[1],
          control  = dat$control[1]
        ))
      }
      
      # ---- Model fit ----
      fit <- tryCatch(
        stats::lm(
          diff_OD_590_740 ~ splines::ns(Hour, df = spline_df),
          data = dat
        ),
        error = function(e) NULL
      )
      
      if (is.null(fit)) {
        return(dplyr::tibble(
          AUC = NA_real_,
          AUC_low = NA_real_,
          AUC_high = NA_real_,
          n_points = n_valid,
          Chemical = dat$Chemical[1],
          control  = dat$control[1]
        ))
      }
      
      fitted_vals <- stats::fitted(fit)
      residuals   <- stats::residuals(fit)
      
      # ---- Observed AUC ----
      auc_obs <- .auc_trapz(dat$Hour, fitted_vals)
      
      # ---- Bootstrap ----
      auc_boot <- replicate(n_boot, {
        y_star <- fitted_vals + sample(residuals, replace = TRUE)
        .auc_trapz(dat$Hour, y_star)
      })
      
      dplyr::tibble(
        AUC      = auc_obs,
        AUC_low  = stats::quantile(auc_boot, 0.025, na.rm = TRUE),
        AUC_high = stats::quantile(auc_boot, 0.975, na.rm = TRUE),
        n_points = n_valid,
        Chemical = dat$Chemical[1],
        control  = dat$control[1]
      )
      
    }) %>%
    dplyr::ungroup()
  
  # ---- Store results ----
  if (store) {
    data$auc_results <- auc_results
    return(data)
  } else {
    return(auc_results)
  }
}