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
#' @importFrom rlang .data
#' 
#' @examples
#' data(processed_data)
#' processed_data <- classify_growth_curve_types(processed_data)
#' processed_data <- annotate_chemical(processed_data, annotation = PM_M01_M02)
#' processed_data <- compute_auc_ci(processed_data)
#' processed_data <- compute_auc_ratio(processed_data)
#' head(processed_data$auc_ratio)
#' 
#' @export
compute_auc_ratio <- function(data,
                              control_labels = c("Negative Control", "No Chemical"),
                              store = TRUE) {
  
  # ---- Validate top-level structure ----
  if (!is.list(data) || is.data.frame(data) ||
      !all(c("auc_results", "meta") %in% names(data))) {
    stop("'data' must be a named list containing 'auc_results' and 'meta',
          as produced by compute_auc_ci().")
  }
  
  if (!is.list(data$meta) || !"variables" %in% names(data$meta)) {
    stop("'data$meta' must be a named list containing 'variables',
          as produced by read_folder().")
  }
  
  # ---- Extract ----
  df        <- data$auc_results
  variables <- data$meta$variables
  
  # ---- Validate auc_results ----
  if (!is.data.frame(df)) {
    stop("'data$auc_results' must be a data.frame.")
  }
  
  if (nrow(df) == 0) {
    stop("'data$auc_results' must contain at least one row.")
  }
  
  # ---- Validate variables ----
  if (!is.character(variables) || length(variables) == 0) {
    stop("'data$meta$variables' must be a non-empty character vector.")
  }
  
  # ---- Validate required columns ----
  required_cols <- c("Plate.Type", "AUC", "AUC_low", "AUC_high", "Chemical")
  missing_cols  <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(paste("'data$auc_results' is missing required columns:",
               paste(missing_cols, collapse = ", ")))
  }
  
  # ---- Validate control_labels ----
  if (!is.character(control_labels) || length(control_labels) == 0) {
    stop("'control_labels' must be a non-empty character vector.")
  }
  
  # ---- Validate store ----
  if (!is.logical(store) || length(store) != 1 || is.na(store)) {
    stop("'store' must be a single logical value (TRUE or FALSE).")
  }
  
  # ---- Warn if no controls found ----
  n_controls <- sum(df$Chemical %in% control_labels, na.rm = TRUE)
  if (n_controls == 0) {
    stop(paste0(
      "None of the 'control_labels' values were found in 'data$auc_results$Chemical'. ",
      "All AUC_ratio values will be NA. ",
      "Provided labels: ", paste(control_labels, collapse = ", ")
    ))
  }
  
  
  
  # ---- Control summary ----
  join_vars <- c("Plate.Type", variables)
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
  
  c# ---- Join + ratio ----
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