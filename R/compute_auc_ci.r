#' Compute Area Under the Curve (AUC) with Bootstrap Confidence Intervals
#'
#' This function estimates the Area Under the Curve (AUC) for time-series data
#' on a per-well basis using spline-based smoothing, followed by residual
#' bootstrap to derive confidence intervals. The function operates on a
#' structured \code{data} object and integrates results back into it.
#'
#' A natural spline model is fitted to each well-specific trajectory, and the
#' AUC is computed from the fitted values using trapezoidal integration.
#' Uncertainty is quantified via residual resampling.
#'
#' @param data A structured list containing:
#' \describe{
#'   \item{data}{Long-format raw measurements (e.g., OD values).}
#'   \item{meta}{Metadata including grouping variables.}
#'   \item{classification}{Well-level annotations (must include \code{well_summary}).}
#' }
#' @param curve_type A character string specifying which curve type to include
#'   in the analysis (default: \code{"growing"}).
#' @param n_boot Integer specifying the number of bootstrap replicates used
#'   for confidence interval estimation (default: 1000).
#' @param spline_df Integer specifying the degrees of freedom for the natural
#'   spline used in model fitting (default: 4).
#' @param seed Integer random seed for reproducibility (default: 123).
#' @param store Logical; if \code{TRUE}, results are stored in
#'   \code{data$$auc_results} (default: \code{TRUE}).
#'
#' @details
#' The function performs the following steps for each well:
#' \enumerate{
#'   \item Merges raw measurements with well-level classifications.
#'   \item Filters data based on the specified curve type.
#'   \item Fits a linear model with a natural spline basis to capture
#'         non-linear temporal trends.
#'   \item Computes the AUC from fitted values using the trapezoidal rule.
#'   \item Applies residual bootstrap by resampling model residuals and
#'         reconstructing synthetic trajectories.
#'   \item Derives percentile-based confidence intervals (2.5\% and 97.5\%).
#' }
#'
#' Wells with insufficient data points or failed model fits return \code{NA}
#' estimates.
#'
#' @return
#' If \code{store = TRUE}, returns the updated \code{data} object.
#' Otherwise, returns a tibble containing:
#' \describe{
#'   \item{AUC}{Estimated area under the fitted curve.}
#'   \item{AUC_low}{Lower bound of the 95\% bootstrap confidence interval.}
#'   \item{AUC_high}{Upper bound of the 95\% bootstrap confidence interval.}
#'   \item{n_points}{Number of observations used per well.}
#' }
#'
#' @import dplyr
#' @importFrom stats lm fitted residuals quantile
#' @importFrom splines ns
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
  
  # ---- Step 1: Join and filter ----
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
  
  # ---- Step 2: Trapezoidal AUC ----
  auc_trapz <- function(x, y) {
    idx <- order(x)
    x <- x[idx]
    y <- y[idx]
    sum(diff(x) * (head(y, -1) + tail(y, -1)) / 2)
  }
  
  # ---- Step 3: Per-well estimation ----
  auc_results <- combined_df %>%
    dplyr::group_by(
      dplyr::across(dplyr::all_of(c("Plate Type", "Well", variables)))
    ) %>%
    dplyr::group_modify(~{
      
      dat <- .x
      
      # ---- Data sufficiency ----
      if (nrow(dat) < 4 || all(is.na(dat$diff_OD_590_740))) {
        return(dplyr::tibble(
          AUC = NA_real_,
          AUC_low = NA_real_,
          AUC_high = NA_real_,
          n_points = nrow(dat),
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
          n_points = nrow(dat),
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
        n_points = nrow(dat),
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