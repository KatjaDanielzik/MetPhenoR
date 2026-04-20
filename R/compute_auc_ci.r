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
#' @export
compute_auc_ci <- function(data,
                           curve_type = "growing",
                           n_boot = 1000,
                           spline_df = 4,
                           seed = 123,
                           store = TRUE) {
  
  set.seed(seed)
  
  # ---- Validation ----
  if (!all(c("data", "meta", "classification") %in% names(data))) {
    stop("data must contain 'data', 'meta', and 'classification'")
  }
  
  if (!"well_summary" %in% names(data$classification)) {
    stop("Run classify_growth_curve_types() first")
  }
  
  raw_data     <- data$data
  well_summary <- data$classification$well_summary
  variables    <- data$meta$variables
  
  # ---- Join and filter ----
  combined_df <- dplyr::left_join(
    raw_data,
    well_summary,
    by = c(variables, "Plate Type", "Well")
  ) %>%
    dplyr::filter(.data$curve_type == curve_type)
  
  # ---- Check required columns ----
  if (!all(c("Chemical", "control") %in% colnames(combined_df))) {
    stop("'Chemical' and/or 'control' not found in data$data")
  }
  
  # ---- Global data sufficiency check ----
  sampling_summary <- combined_df %>%
    dplyr::group_by(
      dplyr::across(dplyr::all_of(c("Plate Type", "Well", variables)))
    ) %>%
    dplyr::summarise(
      n_points = sum(!is.na(diff_OD_590_740)),
      .groups = "drop"
    )
  
  min_points <- min(sampling_summary$n_points)
  max_points <- max(sampling_summary$n_points)
  
  if (min_points < 4) {
    warning(
      paste0(
        "Some wells contain fewer than 4 non-missing observations (min = ",
        min_points,
        "). These wells will return NA AUC estimates."
      )
    )
  }
  
  if (min_points != max_points) {
    warning(
      paste0(
        "Inconsistent number of time points detected across wells: ",
        "min = ", min_points,
        ", max = ", max_points,
        ". This may affect comparability of AUC estimates."
      )
    )
  }
  
  # ---- Trapezoidal AUC ----
  auc_trapz <- function(x, y) {
    idx <- order(x)
    x <- x[idx]
    y <- y[idx]
    sum(diff(x) * (head(y, -1) + tail(y, -1)) / 2)
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
      auc_obs <- auc_trapz(dat$Hour, fitted_vals)
      
      # ---- Bootstrap ----
      auc_boot <- replicate(n_boot, {
        y_star <- fitted_vals + sample(residuals, replace = TRUE)
        auc_trapz(dat$Hour, y_star)
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