#' Compute Area Under the Curve (AUC) with Bootstrap Confidence Intervals
#'
#' This function estimates the Area Under the Curve (AUC) for time-series data
#' on a per-well basis using spline-based smoothing, followed by residual
#' bootstrap to derive confidence intervals.
#'
#' A natural spline model is fitted to each well-specific trajectory, and the
#' AUC is computed from the fitted values using trapezoidal integration.
#' Uncertainty is quantified via residual resampling.
#'
#' @param combined_raw_long A data frame in long format containing raw
#'   time-series measurements (e.g., optical density values).
#' @param well_summary A data frame containing well-level annotations,
#'   including curve classifications.
#' @param variables A character vector specifying grouping variables that
#'   define experimental conditions.
#' @param curve_type A character string specifying which curve type to include
#'   in the analysis (default: \code{"growing"}).
#' @param n_boot Integer specifying the number of bootstrap replicates used
#'   for confidence interval estimation (default: 1000).
#' @param spline_df Integer specifying the degrees of freedom for the natural
#'   spline used in model fitting (default: 4).
#' @param seed Integer random seed for reproducibility (default: 123).
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
#' @return A tibble containing:
#' \describe{
#'   \item{AUC}{Estimated area under the fitted curve.}
#'   \item{AUC_low}{Lower bound of the 95\% bootstrap confidence interval.}
#'   \item{AUC_high}{Upper bound of the 95\% bootstrap confidence interval.}
#'   \item{n_points}{Number of observations used per well.}
#' }
#'
#' @import dplyr
#' @import magrittr
#' @importFrom stats lm
#' @importFrom stats fitted
#' @importFrom stats residuals
#' @importFrom stats quantile
#' @importFrom splines ns
#' 
#' @export
compute_auc_ci <- function(combined_raw_long,
                           well_summary,
                           variables,
                           curve_type = "growing",
                           n_boot = 1000,
                           spline_df = 4,
                           seed = 123) {
  
  set.seed(seed)
  
  # ---- Step 1: Join and filter data ----
  combined_df <- dplyr::left_join(
    combined_raw_long,
    well_summary,
    by = c(variables, "Plate.Type", "Well")
  ) %>%
    dplyr::filter(.data$Curve_type == curve_type)
  
  # ---- Step 2: Trapezoidal AUC function ----
  
  # ---- Step 3: Per-well estimation ----
  auc_ci_residual <- combined_df %>%
    dplyr::group_by(
      dplyr::across(dplyr::all_of(c("Plate.Type", "Well", variables)))
    ) %>%
    dplyr::group_modify(~{
      
      dat <- .x
      
      # ---- Data sufficiency check ----
      if (nrow(dat) < 4 || all(is.na(dat$raw_value))) {
        return(dplyr::tibble(
          AUC = NA_real_,
          AUC_low = NA_real_,
          AUC_high = NA_real_,
          n_points = nrow(dat)
        ))
      }
      
      # ---- Fit spline-based regression model ----
      fit <- tryCatch(
        stats::lm(
          raw_value ~ splines::ns(Hour, df = spline_df),
          data = dat
        ),
        error = function(e) NULL
      )
      
      if (is.null(fit)) {
        return(dplyr::tibble(
          AUC = NA_real_,
          AUC_low = NA_real_,
          AUC_high = NA_real_,
          n_points = nrow(dat)
        ))
      }
      
      fitted_vals <- stats::fitted(fit)
      residuals   <- stats::residuals(fit)
      
      # ---- Observed AUC from fitted trajectory ----
      auc_obs <- auc_trapz(dat$Hour, fitted_vals)
      
      # ---- Residual bootstrap procedure ----
      auc_boot <- replicate(n_boot, {
        y_star <- fitted_vals + sample(residuals, replace = TRUE)
        auc_trapz(dat$Hour, y_star)
      })
      
      # ---- Return estimates ----
      dplyr::tibble(
        AUC      = auc_obs,
        AUC_low  = stats::quantile(auc_boot, 0.025, na.rm = TRUE),
        AUC_high = stats::quantile(auc_boot, 0.975, na.rm = TRUE),
        n_points = nrow(dat)
      )
      
    }) %>%
    dplyr::ungroup()
  
  return(auc_ci_residual)
}