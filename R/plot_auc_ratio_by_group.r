#' Plot AUC ratio heatmap by group
#'
#' Generates a heatmap of AUC ratios grouped by a specified column in annotation,
#' allowing visualization of treatment effects across groups (e.g. pathways, drug targets).
#'
#' @param data A list containing a data frame element \code{auc_ratio}.
#'   The data frame must include columns \code{"Plate.Type"}, \code{"Well"},
#'   \code{"Chemical"}, \code{"AUC_ratio"}, and the specified pathway variable.
#' @param group_var Character string specifying the column to define groups.
#' @param x_var Character string indicating the column to be mapped to the x-axis.
#'  Defaults to first entry in data$meta$variables.
#' @param group_filter Optional character vector specifying group values
#'   to include. If \code{NULL}, all groups are retained.
#' @param scale_type Character string specifying the transformation applied to
#'   AUC ratios. Must be one of \code{"natural"} or \code{"log10"}.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Optionally filters the dataset for selected pathways.
#'   \item Removes control wells without chemical treatment.
#'   \item Constructs a unique identifier per well using plate, well, and chemical.
#'   \item Optionally applies a log10 transformation to AUC ratios.
#'   \item Generates a heatmap using \code{ggplot2::geom_tile()}.
#'   \item Facets the plot by pathway and additional experimental variables.
#' }
#'
#' @return A \code{ggplot} object representing the AUC ratio heatmap by pathway.
#'
#' @importFrom dplyr filter mutate distinct pull
#' @importFrom ggplot2 ggplot aes geom_tile facet_grid scale_fill_gradient2
#' @importFrom ggplot2 labs theme element_blank element_text
#' @importFrom rlang .data
#'
#' @examples
#' data(processed_data)
#' processed_data <- classify_growth_curve_types(processed_data)
#' processed_data <- annotate_chemical(processed_data, annotation = PM_M01_M02)
#' processed_data <- compute_auc_ci(processed_data)
#' processed_data <- compute_auc_ratio(processed_data)
#' processed_data <- annotate_metadata(processed_data, annotation = PM_M01_M02)
#' plot_auc_ratio_by_group(processed_data, 
#'                           group_var = "middle_hierarchy",
#'                           x_var = "treatment", 
#'                           group_filter = "Nucleotide metabolism")
#'
#' @export
plot_auc_ratio_by_group <- function(data,
                                      group_var,
                                      x_var,
                                      group_filter = NULL,
                                      scale_type = c("natural", "log10")) {
  
  
  # ---- Validate input ----
  if (!("auc_ratio" %in% names(data))) {
    stop("'data' must contain an 'auc_ratio' element. Run and check output of function 'compute_auc_ratio()'.")
  }
  
  data_df <- data$auc_ratio
  
  if (!is.data.frame(data_df)) {
    stop("'data$auc_ratio' must be a data.frame.")
  }
  
  scale_type <- match.arg(scale_type)
  
  # ---- Auto-select x_var if NULL ----
  if (is.null(x_var)) {
    if (is.null(data$meta$variables) || length(data$meta$variables) == 0) {
      stop("No 'variables' found in 'data$meta'. Cannot auto-select 'x_var'.",
           " Please provide a valid 'x_var'.")
    }
    
    # Use the first entry
    x_var <- data$meta$variables[1]
    message("Using '", x_var, "' as default 'x_var' (first entry 'data$meta$variables').")
  }
  
  # ---- Validate group_var exists ----
  if (!group_var %in% names(data_df)) {
    stop("Column '", group_var, "' not found in 'data$auc_ratio'.",
         " Available columns: ", paste(names(data_df), collapse = ", "))
  }
  
  # ---- Validate group_filter if provided ----
  if (!is.null(group_filter)) {
    if (!all(group_filter %in% unique(data_df[[group_var]]))) {
      missing <- setdiff(group_filter, unique(data_df[[group_var]]))
      warning("The following values in 'group_filter' were not found in '", group_var, "': ",
              paste(missing, collapse = ", "),
              ". These will be ignored.")
    }
  }
  
  # ---- Prepare variables ----
  variables   <- data$meta$variables
  column_vars <- setdiff(variables, x_var)
  
  # Convert to symbols (handles spaces safely)
  pathway_sym <- sym(group_var)
  x_sym       <- sym(x_var)
  column_syms <- syms(column_vars)
  
  # ---- Optional pathway filtering ----
  if (!is.null(group_filter)) {
    data_df <- data_df %>%
      dplyr::filter(!!pathway_sym %in% group_filter)
  }
  
  # ---- Data cleaning ----
  data_filtered <- data_df %>%
    dplyr::filter(.data$Chemical != "No Chemical") %>%
    dplyr::mutate(
      ID = paste0(.data$Plate.Type, "_", .data$Well, "_", .data$Chemical)
    )
  
  # ---- Apply transformation ----
  if (scale_type == "log10") {
    data_filtered <- data_filtered %>%
      dplyr::mutate(AUC_ratio_plot = log10(.data$AUC_ratio))
    
    midpoint_value <- 0
    legend_title   <- expression(log[10]~"(AUC ratio)")
    
  } else {
    data_filtered <- data_filtered %>%
      dplyr::mutate(AUC_ratio_plot = .data$AUC_ratio)
    
    midpoint_value <- 1
    legend_title   <- "AUC ratio"
  }
  
  # ---- Reverse ID order ----
  id_levels <- data_filtered %>%
    dplyr::distinct(.data$ID) %>%
    dplyr::pull(.data$ID)
  
  data_filtered <- data_filtered %>%
    dplyr::mutate(
      ID = factor(.data$ID, levels = rev(id_levels))
    )
  
  # ---- Plot ----
  p <- ggplot2::ggplot(
    data_filtered,
    ggplot2::aes(
      x = !!x_sym,
      y = .data$ID,
      fill = .data$AUC_ratio_plot
    )
  ) +
    ggplot2::geom_tile(color = "grey90", linewidth = 0.2)
  
  # ---- Faceting (tidy evaluation safe) ----
  if (length(column_syms) > 0) {
    p <- p +
      ggplot2::facet_grid(
        rows = vars(!!pathway_sym),
        cols = vars(!!!column_syms),
        scales = "free_y",
        space  = "free_y",
        labeller = ggplot2::label_wrap_gen(width = 20)
      )
  } else {
    p <- p +
      ggplot2::facet_grid(
        rows = vars(!!pathway_sym),
        scales = "free_y",
        space  = "free_y",
        labeller = ggplot2::label_wrap_gen(width = 20)
      )
  }
  
  # ---- Final styling ----
  p +
    ggplot2::scale_fill_gradient2(
      low = "#0072B2",
      mid = "white",
      high = "#D55E00",
      midpoint = midpoint_value,
      na.value = "grey",
      name = legend_title
    ) +
    ggplot2::labs(
      x = x_var,
      y = "Plate_Well_Chemical",
      title = paste0("AUC Ratio by ", group_var,
                     " (", scale_type, " scale)"),
      subtitle = paste0(
        "AUC ratio = AUC[well] / mean(AUC[control on plate])",
        "\nlight grey: inappropriate curve types, dark grey: NA"
      )
    ) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1),
      axis.text.y = ggplot2::element_text(size = 7),
      strip.text = ggplot2::element_text(face = "bold", size = 10),
      legend.title = ggplot2::element_text(face = "bold"),
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      plot.subtitle = ggplot2::element_text(size = 10),
      legend.position = "right"
    )
}
