#' AUC Plot for Controls and Tween Compounds within plate
#'
#' This function visualizes Area Under the Curve (AUC) estimates for control
#' wells and Tween-treated conditions within a selected plate. The plot includes
#' confidence intervals and highlights the reference range defined by negative
#' controls.
#'
#' Tween compounds are displayed alongside control groups to facilitate
#' comparative assessment of growth responses relative to baseline and
#' positive controls.
#'
#' @param data A data frame containing annotated AUC results, typically the
#'   output of \code{prepare_auc_annotations()}.
#' @param selected_plate A character string specifying the \code{Plate.Type}
#'   to visualize.
#' @param variables A character vector defining experimental grouping variables.
#'   At least two variables are recommended for meaningful faceting.
#' @param tween_labels A character vector specifying the names of Tween
#'   compounds to include in the comparison (default:
#'   \code{c("Tween 20", "Tween 40", "Tween 80")}).
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Filters the dataset for the selected plate and retains only
#'         control wells and specified Tween compounds.
#'   \item Classifies observations into three groups: negative controls,
#'         positive controls, and Tween-treated wells.
#'   \item Computes a reference range based on the minimum and maximum
#'         confidence interval bounds of negative controls within each
#'         experimental condition.
#'   \item Visualizes AUC estimates with horizontal error bars representing
#'         confidence intervals.
#'   \item Overlays vertical reference lines indicating:
#'         \itemize{
#'           \item Lower and upper bounds of the negative control range
#'           \item Zero baseline (AUC = 0)
#'         }
#'   \item Applies faceting based on the specified experimental variables.
#' }
#'
#' This representation enables direct comparison of Tween-induced responses
#' against empirically derived control variability.
#'
#' @return A \code{ggplot2} object displaying AUC estimates, confidence
#'   intervals, and control reference ranges across experimental conditions.
#'   
#' @import dplyr
#' @import magrittr
#' @import ggplot2
#' @importFrom stats as.formula
#'
#' @export
plot_controls_tween <- function(data,
                                selected_plate,
                                variables,
                                tween_labels = c("Tween 20", "Tween 40", "Tween 80")) {
  
  # ---- Input validation ----
  required_cols <- c("Plate.Type", "AUC", "AUC_low", "AUC_high", "Well")
  if (!all(required_cols %in% names(data))) {
    stop("Missing required columns in 'data'.")
  }
  
  if (!"control" %in% names(data)) {
    stop("'control' column is required.")
  }
  
  if (!"Chemical" %in% names(data)) {
    stop("'Chemical' column is required.")
  }
  
  if (length(variables) < 2) {
    stop("'variables' must contain at least two elements.")
  }
  
  # ---- Flexible faceting setup ----
  row_vars <- variables[1:min(2, length(variables))]
  col_vars <- if (length(variables) > 2) variables[3:min(4, length(variables))] else NULL
  
  facet_formula <- if (!is.null(col_vars)) {
    stats::as.formula(
      paste(paste(row_vars, collapse = " + "),
            "~",
            paste(col_vars, collapse = " + "))
    )
  } else {
    stats::as.formula(paste(paste(row_vars, collapse = " + "), "~ ."))
  }
  
  # ---- Step 1: Filter relevant data ----
  plot_df <- dplyr::filter(
    data,
    .data$Plate.Type == selected_plate,
    .data$control %in% c("negative", "positive") |
      .data$Chemical %in% tween_labels
  )
  
  if (nrow(plot_df) == 0) {
    stop("No data available after filtering.")
  }
  
  # ---- Step 2: Assign comparison groups ----
  plot_df <- dplyr::mutate(
    plot_df,
    group_type = dplyr::case_when(
      .data$control %in% c("negative", "positive") ~ .data$control,
      .data$Chemical %in% tween_labels ~ "Tween",
      TRUE ~ NA_character_
    )
  )
  
  # ---- Step 3: Compute negative control reference range ----
  control_ranges <- plot_df %>%
    dplyr::filter(.data$control == "negative") %>%
    dplyr::group_by(dplyr::across(all_of(variables))) %>%
    dplyr::summarise(
      control_lowest  = min(.data$AUC_low, na.rm = TRUE),
      control_highest = max(.data$AUC_high, na.rm = TRUE),
      .groups = "drop"
    )
  
  # ---- Step 4: Merge and prepare labels ----
  plot_df <- dplyr::left_join(
    plot_df,
    control_ranges,
    by = variables
  ) %>%
    dplyr::arrange(dplyr::across(all_of(variables)), .data$group_type, .data$Well) %>%
    dplyr::mutate(
      Chemical_label = paste0(.data$Chemical, " (", .data$Well, ")"),
      group_type = factor(
        .data$group_type,
        levels = c("negative", "positive", "Tween")
      )
    )
  
  # ---- Step 5: Visualization ----
  p <- ggplot2::ggplot(plot_df, ggplot2::aes(y = .data$Chemical_label, x = .data$AUC)) +
    
    ggplot2::geom_errorbarh(
      ggplot2::aes(xmin = .data$AUC_low, xmax = .data$AUC_high),
      height = 0.25,
      linewidth = 0.6
    ) +
    
    ggplot2::geom_point(ggplot2::aes(shape = .data$group_type)) +
    
    ggplot2::scale_shape_manual(
      name = "Group",
      values = c("negative" = 17, "positive" = 15, "Tween" = 16)
    ) +
    
    ggplot2::geom_vline(
      ggplot2::aes(xintercept = .data$control_lowest),
      linetype = "dashed"
    ) +
    
    ggplot2::geom_vline(
      ggplot2::aes(xintercept = .data$control_highest),
      linetype = "dashed"
    ) +
    
    ggplot2::geom_vline(xintercept = 0, color = "blue", linewidth = 0.5) +
    
    ggplot2::labs(
      title = paste("AUC Comparison: Controls vs Tween | Plate:", selected_plate),
      x = "AUC estimate",
      y = NULL
    ) +
    
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = 6),
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      strip.text = ggplot2::element_text(size = 7)
    ) +
    
    ggplot2::facet_grid(
      facet_formula,
      scales = "free_y",
      space = "free_y"
    )
  
  return(p)
}