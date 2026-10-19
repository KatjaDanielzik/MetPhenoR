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
#' @param data A structured list
#' @param selected_plate A character string specifying the \code{Plate.Type}
#'   to visualize.
#' @param variables A character vector defining experimental grouping variables.
#'   At least two variables are recommended for meaningful faceting. Defaults
#'   to data$meta$variables.
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
#' @importFrom rlang .data
#'
#' @examples
#' data(processed_data)
#' processed_data <- annotate_chemical(processed_data, annotation = PM_M01_M02)
#' processed_data <- classify_growth_curve_types(processed_data)
#' processed_data <- compute_auc_ci(processed_data, n_boot = 10)
#' plot_controls_tween_per_plate(processed_data, selected_plate = "PM-M01")
#'
#' @export

plot_controls_tween_per_plate <- function(
  data,
  selected_plate,
  variables = NULL,
  tween_labels = c("Tween 20", "Tween 40", "Tween 80")
) {
  # ---- Validate top-level data structure ----
  if (!is.list(data) || is.data.frame(data) ||
    !"auc_results" %in% names(data)) {
    stop("'data' must be a named list containing 'auc_results',
          as produced by compute_auc_ci().")
  }

  # ---- Validate auc_results ----
  if (!is.data.frame(data$auc_results)) {
    stop("'data$auc_results' must be a data.frame.")
  }

  if (nrow(data$auc_results) == 0) {
    stop("'data$auc_results' must contain at least one row.")
  }

  # ---- Validate variables ----
  if (is.null(variables)) {
    if (!is.list(data$meta) || !"variables" %in% names(data$meta)) {
      stop("'variables' must be provided or exist in 'data$meta$variables'.")
    }
    variables <- data$meta$variables
  }

  if (!is.character(variables) || length(variables) == 0) {
    stop("'variables' must be a non-empty character vector.")
  }

  # ---- Validate selected_plate ----
  if (missing(selected_plate) || is.null(selected_plate)) {
    stop("'selected_plate' must be provided.")
  }

  if (!is.character(selected_plate) || length(selected_plate) != 1) {
    stop("'selected_plate' must be a single character string.")
  }

  # ---- Validate tween_labels ----
  if (!is.character(tween_labels) || length(tween_labels) == 0) {
    stop("'tween_labels' must be a non-empty character vector.")
  }

  # ---- Validate required columns in auc_results ----
  df <- data$auc_results

  required_cols <- c(
    "Plate.Type", "AUC", "AUC_low", "AUC_high",
    "Well", "Chemical", "control"
  )
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(paste(
      "'data$auc_results' is missing required columns:",
      paste(missing_cols, collapse = ", ")
    ))
  }

  # ---- Validate variables exist in auc_results ----
  missing_vars <- setdiff(variables, names(df))
  if (length(missing_vars) > 0) {
    stop(paste(
      "The following 'variables' are not present in 'data$auc_results':",
      paste(missing_vars, collapse = ", ")
    ))
  }

  # ---- Warn if selected_plate not found ----
  if (!selected_plate %in% unique(df$Plate.Type)) {
    warning(paste0(
      "'selected_plate' value '", selected_plate,
      "' was not found in 'data$auc_results$Plate.Type'. ",
      "No data will be plotted."
    ))
  }

  # Flexible faceting
  row_vars <- variables[1:min(2, length(variables))]
  col_vars <- if (length(variables) > 2) variables[3:min(4, length(variables))] else NULL

  facet_formula <- if (!is.null(col_vars)) {
    stats::as.formula(
      paste(
        paste(row_vars, collapse = " + "),
        "~",
        paste(col_vars, collapse = " + ")
      )
    )
  } else {
    stats::as.formula(paste(paste(row_vars, collapse = " + "), "~ ."))
  }

  # Filter relevant data
  plot_df <- dplyr::filter(
    df,
    .data$Plate.Type == selected_plate,
    .data$control %in% c("negative", "positive") |
      .data$Chemical %in% tween_labels
  )

  if (nrow(plot_df) == 0) {
    stop("No data available after filtering.")
  }

  # Assign groups
  plot_df <- dplyr::mutate(
    plot_df,
    group_type = dplyr::case_when(
      .data$control %in% c("negative", "positive") ~ .data$control,
      .data$Chemical %in% tween_labels ~ "Tween",
      TRUE ~ NA_character_
    )
  )

  # Compute negative control reference range
  control_ranges <- plot_df %>%
    dplyr::filter(.data$control == "negative") %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(variables))) %>%
    dplyr::summarise(
      control_lowest = min(.data$AUC_low, na.rm = TRUE),
      control_highest = max(.data$AUC_high, na.rm = TRUE),
      .groups = "drop"
    )

  # Safe join
  common_vars <- intersect(variables, names(control_ranges))

  if (length(common_vars) == 0) {
    stop("No common variables available for joining.")
  }

  plot_df <- dplyr::left_join(
    plot_df,
    control_ranges,
    by = common_vars
  )

  # Label preparation
  plot_df <- plot_df %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(variables)), .data$group_type, .data$Well) %>%
    dplyr::mutate(
      Chemical_label = paste0(.data$Chemical, " (", .data$Well, ")"),
      group_type = factor(
        .data$group_type,
        levels = c("negative", "positive", "Tween")
      )
    )

  # unique reference values
  ref_df <- dplyr::distinct(
    plot_df,
    dplyr::across(dplyr::all_of(variables)),
    .data$control_lowest,
    .data$control_highest
  )

  # Plot
  p <- ggplot2::ggplot(plot_df, ggplot2::aes(y = .data$Chemical_label, x = .data$AUC)) +
    ggplot2::geom_errorbar(
      ggplot2::aes(xmin = .data$AUC_low, xmax = .data$AUC_high),
      height = 0.25,
      linewidth = 0.6
    ) +
    ggplot2::geom_point(
      ggplot2::aes(shape = .data$group_type),
      size = 2
    ) +
    ggplot2::scale_shape_manual(
      name = "Group",
      values = c("negative" = 17, "positive" = 15, "Tween" = 16)
    ) +
    ggplot2::geom_vline(
      data = ref_df,
      ggplot2::aes(xintercept = .data$control_lowest),
      linetype = "dashed"
    ) +
    ggplot2::geom_vline(
      data = ref_df,
      ggplot2::aes(xintercept = .data$control_highest),
      linetype = "dashed"
    ) +
    ggplot2::geom_rect(
      data = ref_df,
      ggplot2::aes(
        xmin = .data$control_lowest,
        xmax = .data$control_highest,
        ymin = -Inf,
        ymax = Inf
      ),
      inherit.aes = FALSE,
      fill = "black",
      alpha = 0.05
    ) +
    ggplot2::geom_vline(
      xintercept = 0,
      color = "blue",
      linewidth = 0.5
    ) +
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
