#' Compute AUC Ratio Relative to Controls
#'
#' This function calculates normalized AUC ratios relative to control wells
#' and integrates the results into the structured \code{data} object,
#' consistent with \code{compute_auc_ci()}.
#'
#' @param data A structured list containing:
#' \describe{
#'   \item{auc_results}{Output from \code{compute_auc_ci()}.}
#'   \item{meta}{Metadata including grouping variables.}
#' }
#' @param control_labels Character vector defining control wells
#'   (default: \code{c("Negative Control", "No Chemical")}).
#' @param store Logical; if \code{TRUE}, results are stored in
#'   \code{data$auc_ratio_results} (default: \code{TRUE}).
#'
#' @return
#' If \code{store = TRUE}, returns updated \code{data} object.
#' Otherwise returns a tibble with AUC ratio calculations.
#'
#' @import dplyr
#' 
#' @examples
#' \dontrun{
#' data <- compute_auc_ratio(data)
#' head(data$auc_ratio)
#' }
#' 
#' @export
compute_auc_ratio <- function(data,
                              control_labels = c("Negative Control", "No Chemical"),
                              store = TRUE) {
  
  # ---- Validation ----
  if (!all(c("auc_results", "meta") %in% names(data))) {
    stop("data must contain 'auc_results' and 'meta'")
  }
  
  df <- data$auc_results
  variables <- data$meta$variables
  
  required_cols <- c("AUC", "AUC_low", "AUC_high", "Chemical")
  missing_cols <- setdiff(required_cols, colnames(df))
  if (length(missing_cols) > 0) {
    stop("Missing required columns in auc_results: ",
         paste(missing_cols, collapse = ", "))
  }
  
  join_vars <- c("Plate Type", variables)
  
  # ---- Control summary ----
  ctrl_summary <- df %>%
    dplyr::filter(
      .data$Chemical %in% control_labels
    ) %>%
    dplyr::distinct(
      dplyr::across(dplyr::all_of(join_vars)),
      .data$AUC, .data$AUC_low, .data$AUC_high
    ) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(join_vars))) %>%
    dplyr::summarise(
      mean_ctrl_AUC      = mean(.data$AUC, na.rm = TRUE),
      sd_ctrl_AUC        = stats::sd(.data$AUC, na.rm = TRUE),
      mean_ctrl_AUC_low  = mean(.data$AUC_low, na.rm = TRUE),
      mean_ctrl_AUC_high = mean(.data$AUC_high, na.rm = TRUE),
      .groups = "drop"
    )
  
  # ---- Join + ratio ----
  auc_ratio_results <- df %>%
    dplyr::left_join(ctrl_summary, by = join_vars) %>%
    dplyr::mutate(
      AUC_ratio = .data$AUC / .data$mean_ctrl_AUC
    )
  
  # ---- Store or return ----
  if (store) {
    data$auc_ratio <- auc_ratio_results
    return(data)
  } else {
    return(auc_ratio_results)
  }
}