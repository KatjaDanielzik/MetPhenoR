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
#' @param target_curve_type Character string specifying which curve type to include
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
                           seed = 123,
                           store = TRUE) {
  
  set.seed(seed)
  
  #Validation
  if (!all(c("data", "meta", "classification") %in% names(data))) {
    stop("data must contain 'data', 'meta', and 'classification'")
  }
  
  if (!"well_summary" %in% names(data$classification)) {
    stop("Run classify_growth_curve_types() first")
  }
  
  variable <- NULL
  raw_data     <- data$data
  well_summary <- data$classification$well_summary
  variables    <- data$meta$variables
  
  #Join and filter
  combined_df <- dplyr::left_join(
    raw_data,
    well_summary,
    by = c(variables, "Plate Type", "Well")
  ) %>%
    dplyr::filter(.data$curve_type == target_curve_type)
  
  #Check required columns
  if (!all(c("Chemical", "control") %in% colnames(combined_df))) {
    stop("'Chemical' and/or 'control' not found in data$data")
  }
  
  #Global data sufficiency check
  sampling_summary <- combined_df %>%
    dplyr::group_by(.data$PlateId, .data$Well) %>%
    dplyr::summarise(
      n_points = sum(!is.na(.data$diff_OD_590_740)),
      .groups = "drop"
    )
  
  # Get Hour sets per well
  hour_sets <- combined_df %>%
    dplyr::group_by(PlateId, Well) %>%
    dplyr::summarise(
      hour_list = list(sort(unique(Hour))),
      .groups = "drop"
    )
  
  # Compare within each plate
  hour_consistency <- hour_sets %>%
    dplyr::group_by(PlateId) %>%
    dplyr::summarise(
      n_unique_hour_sets = dplyr::n_distinct(hour_list),
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
    dplyr::group_by(PlateId) %>%
    dplyr::summarise(
      n_timepoints = dplyr::n_distinct(Hour),
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
      dplyr::across(dplyr::all_of(c("Plate Type", "Well", variables)))
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