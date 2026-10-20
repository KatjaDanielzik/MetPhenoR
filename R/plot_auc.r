#' Plot AUC Profiles by Plate and Experimental Conditions
#'
#' Generates faceted visualizations of area-under-the-curve (AUC) estimates
#' for a selected plate across experimental conditions. The function highlights
#' wells that deviate from reference limits derived from negative controls and
#' overlays these limits for visual comparison.
#'
#' Control limits are computed within each facet as the minimum and maximum
#' bounds of the AUC confidence intervals from negative control wells.
#'
#' @param data A list containing processed results. Must include
#'   \code{auc_results} (data frame) and \code{meta$variables}.
#' @param selected_plate Character scalar specifying the plate to analyse.
#' @param facet_vars Character vector (length \eqn{\le 2}) specifying variables
#'   used for faceting. Defaults to the first two variables in
#'   \code{data$meta$variables}.
#'
#' @return Invisibly returns a named list of \code{ggplot} objects, each
#'   corresponding to a unique combination of non-faceted grouping variables.
#'
#' @details
#' The function performs the following steps:
#' \itemize{
#'   \item Filters data for the selected plate.
#'   \item Splits data by non-faceted experimental variables.
#'   \item Classifies wells as negative control, positive control, or test wells.
#'   \item Computes facet-specific control limits from negative controls.
#'   \item Identifies wells outside control limits based on AUC confidence bounds.
#'   \item Orders wells globally by mean AUC for consistent visualization.
#'   \item Generates faceted \code{ggplot2} visualizations with:
#'     \itemize{
#'       \item AUC estimates and confidence intervals,
#'       \item shaded control regions and boundary lines,
#'       \item color-coded response categories.
#'     }
#' }
#'
#' Response categories are encoded as:
#' \itemize{
#'   \item \strong{Red}: below control limits
#'   \item \strong{Grey}: within control limits
#'   \item \strong{Green}: above control limits
#'   \item \strong{Black}: control wells
#' }
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom rlang .data
#'
#' @examples
#' data(processed_data)
#' processed_data <- annotate_chemical(processed_data, annotation = PM_M01_M02)
#' processed_data <- classify_growth_curve_types(processed_data)
#' processed_data <- compute_auc_ci(processed_data, n_boot = 10)
#' plot_auc(processed_data, selected_plate = "PM-M01")
#'
#' @export
plot_auc <- function(data,
                     selected_plate,
                     facet_vars = NULL) {
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

  # ---- Validate meta and variables ----
  if (!is.list(data$meta) || !"variables" %in% names(data$meta)) {
    stop("'data$meta' must be a named list containing 'variables',
          as produced by read_folder().")
  }

  variables <- data$meta$variables

  if (!is.character(variables) || length(variables) == 0) {
    stop("'data$meta$variables' must be a non-empty character vector.")
  }

  if (length(variables) < 2) {
    stop("'data$meta$variables' must contain at least 2 variables for faceting.")
  }

  # ---- Validate required columns in auc_results ----
  df <- data$auc_results

  required_cols <- c(
    "Plate.Type", "Well", "Chemical",
    "AUC", "AUC_low", "AUC_high", "control"
  )
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(paste(
      "'data$auc_results' is missing required columns:",
      paste(missing_cols, collapse = ", ")
    ))
  }

  # ---- Validate selected_plate ----
  if (missing(selected_plate) || is.null(selected_plate)) {
    stop("'selected_plate' must be provided.")
  }

  if (!is.character(selected_plate) || length(selected_plate) != 1) {
    stop("'selected_plate' must be a single character string.")
  }

  if (!selected_plate %in% unique(df$Plate.Type)) {
    stop(paste0(
      "'selected_plate' value '", selected_plate,
      "' was not found in 'data$auc_results$Plate.Type'. ",
      "No data will be plotted."
    ))
  }

  # ---- Validate facet_vars ----
  if (!is.null(facet_vars)) {
    if (!is.character(facet_vars) || length(facet_vars) == 0) {
      stop("'facet_vars' must be a non-empty character vector or NULL.")
    }
    if (length(facet_vars) > 2) {
      stop("'facet_vars' must contain at most 2 variables.")
    }
    missing_facet_vars <- setdiff(facet_vars, names(df))
    if (length(missing_facet_vars) > 0) {
      stop(paste(
        "The following 'facet_vars' are not present in 'data$auc_results':",
        paste(missing_facet_vars, collapse = ", ")
      ))
    }
  }

  split_vars <- setdiff(variables, facet_vars)

  # set facet_vars
  if (is.null(facet_vars)) {
    facet_vars <- variables[1:2]
  }

  # Colors
  color_values <- c(
    "Below control limits"  = "red",
    "Within control limits" = "gray",
    "Above control limits"  = "green",
    "Control wells"         = "black"
  )

  # Filter
  df <- df %>%
    dplyr::filter(.data$Plate.Type == selected_plate)

  if (nrow(df) == 0) {
    stop("No data for selected plate.")
  }

  # Global ordering
  global_levels <- df %>%
    dplyr::mutate(
      FacetLabel = sprintf("%s (%s)", .data$Chemical, .data$Well)
    ) %>%
    dplyr::group_by(.data$FacetLabel) %>%
    dplyr::summarise(
      mean_auc = mean(.data$AUC, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(.data$mean_auc) %>%
    dplyr::pull(.data$FacetLabel)

  # Split combinations
  split_combinations <- df %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(split_vars)))

  plots <- vector("list", nrow(split_combinations))

  # Loop
  for (i in seq_len(nrow(split_combinations))) {
    split_values <- as.list(split_combinations[i, , drop = FALSE])
    names(split_values) <- split_vars

    plot_data <- df

    for (v in split_vars) {
      plot_data <- plot_data %>%
        dplyr::filter(.data[[v]] == split_values[[v]])
    }

    # Control classification
    plot_data <- plot_data %>%
      dplyr::mutate(
        control_type = dplyr::case_when(
          .data$control == "negative" ~ "negative",
          .data$control == "positive" ~ "positive",
          TRUE ~ "none"
        )
      )

    # Control limits
    control_limits <- plot_data %>%
      dplyr::filter(.data$control_type == "negative") %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(facet_vars))) %>%
      dplyr::summarise(
        control_lowest = min(.data$AUC_low, na.rm = TRUE),
        control_highest = max(.data$AUC_high, na.rm = TRUE),
        .groups = "drop"
      )

    plot_data <- dplyr::left_join(plot_data, control_limits, by = facet_vars)

    # Color categories
    plot_data <- plot_data %>%
      dplyr::mutate(
        color_label = dplyr::case_when(
          .data$control_type != "none" ~ "Control wells",
          !is.na(.data$AUC_low) & !is.na(.data$control_highest) & .data$AUC_low > .data$control_highest ~ "Above control limits",
          !is.na(.data$AUC_high) & !is.na(.data$control_lowest) & .data$AUC_high < .data$control_lowest ~ "Below control limits",
          TRUE ~ "Within control limits"
        ),
        color_label = factor(.data$color_label, levels = names(color_values))
      )

    # Labels
    plot_data <- plot_data %>%
      dplyr::mutate(
        FacetLabel = sprintf("%s (%s)", .data$Chemical, .data$Well),
        FacetLabel = factor(.data$FacetLabel, levels = global_levels)
      )

    # Limits data (clean reference dataset)
    limits_data <- plot_data %>%
      dplyr::distinct(
        dplyr::across(dplyr::all_of(facet_vars)),
        .data$control_lowest,
        .data$control_highest
      )

    # Subtitle
    subtitle_text <- paste(
      paste(names(split_values), split_values, sep = "="),
      collapse = " | "
    )

    # Plot
    p <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(x = .data$AUC, y = .data$FacetLabel)
    ) +

      # Shaded control region
      ggplot2::geom_rect(
        data = limits_data,
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

      # Control lines
      ggplot2::geom_vline(
        data = limits_data,
        ggplot2::aes(xintercept = .data$control_lowest),
        linetype = "dashed",
        inherit.aes = FALSE
      ) +
      ggplot2::geom_vline(
        data = limits_data,
        ggplot2::aes(xintercept = .data$control_highest),
        linetype = "dashed",
        inherit.aes = FALSE
      ) +

      # Data
      ggplot2::geom_errorbar(
        ggplot2::aes(
          xmin = .data$AUC_low,
          xmax = .data$AUC_high,
          color = .data$color_label
        ),
        width = 0.25
      ) +
      ggplot2::geom_point(
        ggplot2::aes(
          color = .data$color_label,
          shape = .data$control_type
        ),
        size = 2
      ) +
      ggplot2::scale_y_discrete(drop = FALSE) +
      ggplot2::scale_color_manual(
        values = color_values,
        drop = FALSE
      ) +
      ggplot2::scale_shape_manual(
        values = c("none" = 16, "negative" = 17, "positive" = 15)
      ) +
      ggplot2::facet_grid(
        stats::as.formula(paste(facet_vars[2], "~", facet_vars[1])),
        scales = "free_y"
      ) +
      ggplot2::geom_vline(xintercept = 0, color = "blue", linewidth = 0.5) +
      ggplot2::labs(
        title = "AUC",
        subtitle = paste("Plate:", selected_plate, "|", subtitle_text),
        x = "AUC estimate",
        y = NULL,
        color = "Response category"
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        axis.text.y = ggplot2::element_text(size = 6),
        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = ggplot2::element_text(hjust = 0.5)
      )

    plots[[i]] <- p
  }

  return(plots)
}
