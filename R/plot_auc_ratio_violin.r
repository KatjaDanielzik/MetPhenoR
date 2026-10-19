#' Plot AUC ratio distribution as violin plots
#'
#' Visualizes the distribution of AUC ratios across hierarchical groupings
#' using violin plots combined with point distributions.
#'
#' @param data A list containing a data frame element \code{auc_ratio}.
#'   The data frame must include columns \code{"AUC_ratio"}, the specified
#'   hierarchy variable, and the specified color variable.
#' @param group_var Character string specifying the variable used for
#'   grouping along the y-axis.
#' @param group_filter Optional character vector specifying values of
#'   \code{group_var} to include. If \code{NULL}, all values are retained.
#' @param color_var Character string specifying the variable used for color
#'   and fill aesthetics. Defaults to first entry in data$meta$variables
#' @param scale_type Character string specifying the x-axis scale. Must be
#'   one of \code{"natural"} or \code{"log10"}.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Optionally filters the dataset based on the hierarchy variable.
#'   \item Removes invalid AUC ratio values (NA, NaN, Inf, and non-positive values).
#'   \item Generates violin plots with overlaid point distributions
#'         using \code{ggforce::geom_sina()}.
#'   \item Applies either a natural or log10 scale to the x-axis.
#'   \item Facets the plot by the hierarchy variable and additional variables.
#' }
#'
#' @return A \code{ggplot} object representing the distribution of AUC ratios.
#'
#' @importFrom dplyr filter
#' @import ggplot2
#' @importFrom ggforce geom_sina
#' @importFrom scales label_number
#' @importFrom rlang .data
#' 
#' 
#' @examples
#' processed_data <- classify_growth_curve_types(processed_data)
#' processed_data <- annotate_chemical(processed_data, annotation = PM_M01_M02)
#' processed_data <- compute_auc_ci(processed_data)
#' processed_data <- compute_auc_ratio(processed_data)
#' processed_data <- annotate_metadata(processed_data, annotation = PM_M01_M02)
#' plot_auc_ratio_violin(processed_data, 
#'                       group_var = "middle_hierarchy", 
#'                       group_filter = "Nucleotide metabolism", 
#'                       color_var = "treatment")
#'
#' @export

plot_auc_ratio_violin <- function(data,
                                  group_var,
                                  group_filter = NULL,
                                  color_var,
                                  scale_type = c("natural", "log10")) {
  
  # ---- Validate input ----
  if (!("auc_ratio" %in% names(data))) {
    stop("'data' must contain an 'auc_ratio' element. Run and check output of function 'compute_auc_ratio()'.")
  }
  
  variable <- NULL
  data_df <- data$auc_ratio
  
  if (!is.data.frame(data_df)) {
    stop("'data$auc_ratio' must be a data.frame.")
  }
  
  # ---- Validate group_var exists ----
  if (!group_var %in% names(data_df)) {
    stop("Column '", group_var, "' not found in 'data$auc_ratio'.",
         " Available columns: ", paste(names(data_df), collapse = ", "))
  }
  
  # ---- Auto-select color_var if NULL ----
  if (is.null(color_var)) {
    if (is.null(data$meta$variables) || length(data$meta$variables) == 0) {
      stop("No 'variables' found in 'data$meta'. Cannot auto-select 'color_var'.",
           " Please provide a valid 'color_var'.")
    }
    color_var <- data$meta$variables[1]
    message("No 'color_var' specified. Using '", color_var, "' as default 'color_var' (first entry in 'data$meta$variables').")
  }
  
  # ---- Validate group_filter if provided ----
  if (!is.null(group_filter)) {
    if (!all(group_filter %in% unique(data_df[[group_var]]))) {
      missing <- setdiff(group_filter, unique(data_df[[group_var]]))
      warning("The following values in 'group_filter' were not found in '",
              group_var, "': ", paste(missing, collapse = ", "),
              ". These will be ignored.")
    }
  }
  
  scale_type <- match.arg(scale_type)
  
  # ---- Convert to symbols (FIX for spaces!) ----
  pathway_sym <- sym(group_var)
  color_sym   <- sym(color_var)
  
  variables   <- data$meta$variables
  column_vars <- setdiff(variables, c(group_var, color_var))
  column_syms  <- syms(column_vars)
  
  # ---- Optional filtering ----
  if (!is.null(group_filter)) {
    data_df <- data_df %>%
      dplyr::filter(!!pathway_sym %in% group_filter)
  }
  
  # ---- Remove invalid AUC ratios ----
  data_filtered <- data_df %>%
    dplyr::filter(
      !is.na(.data$AUC_ratio),
      !is.nan(.data$AUC_ratio),
      !is.infinite(.data$AUC_ratio),
      .data$AUC_ratio > 0
    )
  
  if (nrow(data_filtered) == 0) {
    stop("No valid AUC_ratio values available for plotting.")
  }
  
  # ---- X transformation ----
  if (scale_type == "log10") {
    data_filtered <- data_filtered %>%
      mutate(AUC_ratio_plot = log10(.data$AUC_ratio))
    
    x_scale <- scale_x_continuous(
      labels = scales::label_number(accuracy = 0.01)
    )
    
    x_label <- "AUC ratio (log10 scale)"
    
  } else {
    data_filtered <- data_filtered %>%
      mutate(AUC_ratio_plot = .data$AUC_ratio)
    
    x_scale <- scale_x_continuous(
      labels = scales::label_number(accuracy = 0.01)
    )
    
    x_label <- "AUC ratio"
  }
  
  # ---- Legend title ----
  legend_title <- gsub("_", " ", color_var)
  
  # ---- Plot ----
  p <- ggplot(
    data_filtered,
    aes(
      x = .data$AUC_ratio_plot,
      y = !!pathway_sym,
      color = !!color_sym,
      fill  = !!color_sym
    )
  ) +
    geom_violin(
      trim = FALSE,
      scale = "count",
      linewidth = 0.3,
      alpha = 0.25
    ) +
    ggforce::geom_sina(
      size = 0.6,
      alpha = 0.6,
      maxwidth = 0.7,
      scale = "count",
    ) +
    geom_vline(
      xintercept = ifelse(scale_type == "log10", 0, 1),
      colour = "black",
      linewidth = 0.6
    )
  
  # ---- Faceting (FIXED) ----
  if (length(column_syms) > 0) {
    p <- p +
      facet_grid(
        rows = vars(!!pathway_sym),
        cols = vars(!!!column_syms),
        scales = "free_y",
        space  = "free_y"
      )
  } else {
    p <- p +
      facet_grid(
        rows = vars(!!pathway_sym),
        scales = "free_y",
        space  = "free_y"
      )
  }
  
  # ---- Scales ----
  p +
    x_scale +
    ggplot2::scale_color_viridis_d(option = "turbo", name = gsub("_", " ", color_var)) +
    ggplot2::scale_fill_viridis_d(option = "turbo",  name = gsub("_", " ", color_var)) +
    
    # ---- Labels ----
  labs(
    x = x_label,
    y = group_var,
    title = paste0("Distribution of AUC ratios per ", group_var)
  ) +
    
    # ---- Theme ----
  theme_bw(base_size = 11) +
    theme(
      legend.position = "right",
      legend.title = element_text(face = "bold"),
      strip.text = element_text(face = "bold", size = 9),
      strip.text.y = element_blank(),
      panel.spacing = grid::unit(0.6, "lines"),
      axis.text.y = element_text(size = 9),
      axis.text.x = element_text(size = 9),
      plot.title = element_text(face = "bold", hjust = 0.5)
    )
}