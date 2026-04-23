#' Plot Controls vs Tween Across All Plates (with Control Reference Range)
#'
#' Similar to plot_controls_tween_per_plate but compares across all plates.
#' Includes negative control reference ranges per facet.
#'
#' @param data A list 
#' @param variables Character vector of column names used for faceting.
#'   If NULL, uses \code{data$meta$variables}.
#'
#' @return A ggplot object.
#'
#' @importFrom dplyr filter mutate case_when group_by summarise across all_of arrange left_join
#' @import ggplot2 
#' @importFrom rlang .data
#' 
#' 
#' @examples
#' data(processed_data)
#' processed_data <- annotate_chemical(processed_data, annotation = PM_M01_M02)
#' processed_data <- classify_growth_curve_types(processed_data)
#' processed_data <- compute_auc_ci(processed_data)
#' plot_controls_tween_all_plate(processed_data)
#' 
#' @export
plot_controls_tween_all_plate <- function(data, variables = NULL) {
  
  # Validate input
  if (!is.list(data) || is.null(data$auc_results)) {
    stop("Input 'data' must be a list containing 'auc_results'.")
  }
  
  if (is.null(variables)) {
    if (!is.null(data$meta) && !is.null(data$meta$variables)) {
      variables <- data$meta$variables
    } else {
      stop("'variables' must be provided or exist in data$meta$variables.")
    }
  }
  
  df <- data$auc_results
  
  required_cols <- c("AUC", "AUC_low", "AUC_high",
                     "Well", "Chemical", "control", "Plate Type")
  
  if (!all(required_cols %in% names(df))) {
    stop("Missing required columns in 'auc_results'.")
  }
  
  if (!all(variables %in% names(df))) {
    stop("Some 'variables' are not present in the dataset.")
  }
  
  #Faceting setup
  row_vars <- variables[seq_len(min(2, length(variables)))]
  col_vars <- if (length(variables) > 2) variables[3:min(4, length(variables))] else NULL
  
  facet_formula <- if (!is.null(col_vars)) {
    stats::as.formula(
      paste(paste(row_vars, collapse = " + "), "~", paste(col_vars, collapse = " + "))
    )
  } else {
    stats::as.formula(paste(paste(row_vars, collapse = " + "), "~ ."))
  }
  
  #Filter
  plot_df <- df %>%
    dplyr::filter(
      .data$control %in% c("negative", "positive") |
        .data$Chemical %in% c("Tween 20", "Tween 40", "Tween 80")
    )
  
  if (nrow(plot_df) == 0) {
    stop("No data available after filtering.")
  }
  
  #Group labels
  plot_df <- plot_df %>%
    dplyr::mutate(
      group_type = dplyr::case_when(
        .data$control %in% c("negative", "positive") ~ .data$control,
        .data$Chemical %in% c("Tween 20", "Tween 40", "Tween 80") ~ "Tween",
        TRUE ~ NA_character_
      )
    )
  
  #Control ranges
  control_ranges <- plot_df %>%
    dplyr::filter(.data$control == "negative") %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(variables))) %>%
    dplyr::summarise(
      control_lowest  = min(.data$AUC_low, na.rm = TRUE),
      control_highest = max(.data$AUC_high, na.rm = TRUE),
      .groups = "drop"
    )
  
  #Join
  plot_df <- dplyr::left_join(plot_df, control_ranges, by = variables)
  
  #Labels
  plot_df <- plot_df %>%
    dplyr::arrange(
      dplyr::across(dplyr::all_of(variables)),
      .data$`Plate Type`,
      .data$group_type,
      .data$Well
    ) %>%
    dplyr::mutate(
      Chemical_label = sprintf("%s (%s)", .data$Chemical, .data$Well),
      group_type = factor(.data$group_type,
                          levels = c("negative", "positive", "Tween"))
    )
  
  #Plot
  p <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(
      y = .data$Chemical_label,
      x = .data$AUC,
      color = .data$`Plate Type`
    )
  ) +
    
    ggplot2::geom_errorbar(
      ggplot2::aes(xmin = .data$AUC_low, xmax = .data$AUC_high),
      height = 0.25,
      linewidth = 0.6,
      position = ggplot2::position_dodge(width = 0.4)
    ) +
    
    ggplot2::geom_point(
      ggplot2::aes(shape = .data$group_type),
      position = ggplot2::position_dodge(width = 0.4),
      size = 2
    ) +
    
    ggplot2::scale_shape_manual(
      name = "Group",
      values = c("negative" = 17, "positive" = 15, "Tween" = 16)
    ) +
    
    ggplot2::geom_vline(
      data = control_ranges,
      ggplot2::aes(xintercept = .data$control_lowest),
      linetype = "dashed"
    ) +
    
    ggplot2::geom_vline(
      data = control_ranges,
      ggplot2::aes(xintercept = .data$control_highest),
      linetype = "dashed"
    ) +
    
    ggplot2::geom_vline(
      xintercept = 0,
      color = "blue",
      linewidth = 0.5
    ) +
    
    ggplot2::labs(
      title = "AUC Comparison: Controls vs Tween Across Plates",
      x = "AUC estimate",
      y = NULL,
      color = "Plate Type"
    ) +
    
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = 6),
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      legend.position = "right",
      strip.text = ggplot2::element_text(size = 7)
    ) +
    
    ggplot2::facet_grid(
      facet_formula,
      scales = "free_y",
      space = "free_y"
    )
  
  return(p)
}