#' Plot AUC Profiles per Plate and Experimental Condition
#'
#' Generates AUC-based visualizations for a selected plate across combinations
#' of experimental grouping variables. The function highlights wells that fall
#' outside control-derived reference limits and overlays these limits for
#' visual comparison.
#'
#' Control limits are computed from negative control wells within each facet
#' level, and are displayed as shaded regions and dashed boundary lines.
#'
#' @param data A list containing processed analysis results. Must include
#'   \code{auc_results} (data frame) and optionally \code{meta$variables}.
#' @param selected_plate Character string specifying the plate to analyze.
#' @param variables Character vector of grouping variables used to define
#'   experimental conditions. If \code{NULL}, variables are retrieved from
#'   \code{data$meta$variables}.
#' @param facet_variable Character string specifying the variable used for
#'   faceting. Defaults to the last element of \code{variables}.
#'
#' @return Invisibly returns a named list of \code{ggplot} objects, where each
#'   plot corresponds to a unique combination of grouping variables.
#'
#' @details
#' The function performs the following steps:
#' \itemize{
#'   \item Filters data for the selected plate and grouping combinations.
#'   \item Classifies wells into control types (negative, positive, none).
#'   \item Computes control reference limits (min/max) from negative controls.
#'   \item Identifies wells outside control limits based on AUC confidence bounds.
#'   \item Applies special inclusion rules for predefined wells (e.g., PM-M02).
#'   \item Orders wells within each facet by AUC magnitude.
#'   \item Generates faceted plots with error bars, control overlays, and annotations.
#' }
#'
#' Color encoding:
#' \itemize{
#'   \item Red: Below control limits
#'   \item Grey: Within control limits
#'   \item Green: Above control limits
#'   \item Black: Control wells
#' }
#'
#' @importFrom dplyr filter mutate case_when distinct across all_of group_by summarise ungroup arrange row_number pull left_join semi_join bind_rows
#' @import ggplot2
#' @importFrom rlang sym .data
#' 
#' @examples
#' \dontrun{
#' auc_plots <- plot_auc(data, selected_plate = "PM-M11")
#' print(auc_plots)
#' }
#' 
#' @export

plot_auc <- function(data,
                     selected_plate,
                     variables = NULL,
                     facet_variable = NULL) {
  
  # ---- Validate inputs ----
  if (!is.list(data) || is.null(data$auc_results)) {
    stop("`data` must be a list containing an `auc_results` data frame.")
  }
  
  if (!is.character(selected_plate) || length(selected_plate) != 1) {
    stop("`selected_plate` must be a single character string.")
  }
  
  df <- data$auc_results
  
  # ---- Resolve variables ----
  if (is.null(variables)) {
    if (!is.null(data$meta) && !is.null(data$meta$variables)) {
      variables <- data$meta$variables
    } else {
      stop("`variables` must be provided or available in `data$meta$variables`.")
    }
  }
  
  # Ensure variables exist
  if (!all(variables %in% names(df))) {
    stop("Some `variables` are not present in `data$auc_results`.")
  }
  
  # ---- Resolve facet variable ----
  if (is.null(facet_variable)) {
    facet_variable <- tail(variables, 1)
  }
  
  if (!facet_variable %in% names(df)) {
    stop("`facet_variable` must exist in `data$auc_results`.")
  }
  
  facet_sym <- rlang::sym(facet_variable)
  
  # Use up to two grouping variables
  grouping_vars <- variables[seq_len(min(2, length(variables)))]
  
  # ---- Generate combinations ----
  combinations <- df %>%
    dplyr::filter(.data$`Plate Type` == selected_plate) %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(grouping_vars)))
  
  if (nrow(combinations) == 0) {
    stop("No data available for the selected plate.")
  }
  
  # ---- Color scheme ----
  color_values <- c(
    "Below control limits"  = "red",
    "Within control limits" = "gray",
    "Above control limits"  = "green",
    "Control wells"         = "black"
  )
  
  auc_plots <- vector("list", nrow(combinations))
  
  # ---- Main loop ----
  for (i in seq_len(nrow(combinations))) {
    
    filter_values <- as.list(combinations[i, , drop = FALSE])
    names(filter_values) <- grouping_vars
    
    # ---- Subset data ----
    filtered_data <- df %>%
      dplyr::filter(.data$`Plate Type` == selected_plate)
    
    for (var in names(filter_values)) {
      filtered_data <- filtered_data %>%
        dplyr::filter(.data[[var]] == filter_values[[var]])
    }
    
    # ---- Control classification ----
    filtered_data <- filtered_data %>%
      dplyr::mutate(
        control_type = dplyr::case_when(
          .data$control == "negative" ~ "negative",
          .data$control == "positive" ~ "positive",
          TRUE ~ "none"
        )
      )
    
    # ---- Compute control limits ----
    control_limits <- filtered_data %>%
      dplyr::filter(.data$control_type == "negative") %>%
      dplyr::group_by(!!facet_sym) %>%
      dplyr::summarise(
        control_lowest  = if (all(is.na(.data$AUC_low))) NA_real_ else min(.data$AUC_low, na.rm = TRUE),
        control_highest = if (all(is.na(.data$AUC_high))) NA_real_ else max(.data$AUC_high, na.rm = TRUE),
        .groups = "drop"
      )
    
    filtered_data <- dplyr::left_join(filtered_data, control_limits, by = facet_variable)
    
    # ---- Identify relevant wells ----
    auc_filtered <- filtered_data %>%
      dplyr::filter(
        .data$control_type != "none" |
          (!is.na(.data$AUC_high) & !is.na(.data$control_lowest)  & .data$AUC_high < .data$control_lowest) |
          (!is.na(.data$AUC_low)  & !is.na(.data$control_highest) & .data$AUC_low  > .data$control_highest)
      )
    
    # ---- Special-case inclusion (PM-M02) ----
    special_wells <- c("A04", "A05", "A06")
    
    special_keys <- filtered_data %>%
      dplyr::filter(.data$`Plate Type` == "PM-M02", .data$Well %in% special_wells) %>%
      dplyr::distinct(.data$Well)
    
    keep_keys <- auc_filtered %>%
      dplyr::distinct(.data$Well) %>%
      dplyr::bind_rows(special_keys) %>%
      dplyr::distinct()
    
    plot_data <- filtered_data %>%
      dplyr::semi_join(keep_keys, by = "Well")
    
    # ---- Assign color categories ----
    plot_data <- plot_data %>%
      dplyr::mutate(
        color_label = dplyr::case_when(
          .data$control_type != "none" ~ "Control wells",
          !is.na(.data$AUC_low)  & !is.na(.data$control_highest) & .data$AUC_low  > .data$control_highest ~ "Above control limits",
          !is.na(.data$AUC_high) & !is.na(.data$control_lowest)  & .data$AUC_high < .data$control_lowest  ~ "Below control limits",
          TRUE ~ "Within control limits"
        )
      )
    
    # ---- Ordering within facets ----
    plot_data <- plot_data %>%
      dplyr::group_by(!!facet_sym) %>%
      dplyr::arrange(.data$AUC, .by_group = TRUE) %>%
      dplyr::mutate(order_rank = dplyr::row_number()) %>%
      dplyr::ungroup()
    
    # ---- Labels ----
    plot_data <- plot_data %>%
      dplyr::mutate(
        FacetLabel = sprintf("%s (%s)", .data$Chemical, .data$Well)
      )
    
    levels_vec <- plot_data %>%
      dplyr::arrange(!!facet_sym, .data$order_rank) %>%
      dplyr::pull(.data$FacetLabel) %>%
      unique()
    
    plot_data <- plot_data %>%
      dplyr::mutate(FacetLabel = factor(.data$FacetLabel, levels = levels_vec))
    
    limits_data <- plot_data %>%
      dplyr::distinct(.data[[facet_variable]], .data$control_lowest, .data$control_highest)
    
    facet_formula <- stats::as.formula(paste("~", facet_variable))
    
    # ---- Plot ----
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(y = .data$FacetLabel, x = .data$AUC)) +
      ggplot2::geom_errorbar(
        ggplot2::aes(xmin = .data$AUC_low, xmax = .data$AUC_high, color = .data$color_label),
        height = 0.25, linewidth = 0.6, na.rm = TRUE
      ) +
      ggplot2::geom_point(
        ggplot2::aes(color = .data$color_label, shape = .data$control_type),
        size = 2, na.rm = TRUE
      ) +
      ggplot2::scale_color_manual(values = color_values) +
      ggplot2::scale_shape_manual(values = c("none" = 16, "negative" = 17, "positive" = 15)) +
      ggplot2::labs(
        title = "AUC response",
        subtitle = paste(
          "Plate:", selected_plate,
          "|",
          paste(names(filter_values), filter_values, sep = "=", collapse = " | ")
        ),
        x = "AUC estimate",
        y = NULL
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        axis.text.y = ggplot2::element_text(size = 6),
        plot.title  = ggplot2::element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = ggplot2::element_text(hjust = 0.5)
      ) +
      ggplot2::facet_wrap(facet_formula, scales = "free_y") +
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
      ggplot2::geom_vline(xintercept = 0, color = "blue", linewidth = 0.5)
    
    auc_plots[[i]] <- p
  }
  
  return(auc_plots)
}

