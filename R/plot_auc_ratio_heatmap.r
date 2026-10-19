#' Plot AUC ratio heatmap
#'
#' Generates a heatmap visualization of AUC ratios across wells, chemicals,
#' and experimental conditions using the \code{auc_ratio} element of the input data.
#'
#' @param data A list containing a data frame element \code{auc_ratio}.
#'   The data frame must include columns \code{"Plate.Type"}, \code{"Well"},
#'   \code{"Chemical"}, and \code{"AUC_ratio"}.
#' @param plate_type Character vector specifying one or more plate types to include.
#' @param x_var Character string indicating the column to be mapped to the x-axis.
#'   Defaults to the first entry in data$meta$variables
#' @param scale_type Character string specifying the transformation applied to
#'   AUC ratios. Must be one of \code{"natural"} or \code{"log10"}.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Filters the dataset for the selected plate types.
#'   \item Constructs a unique identifier per well using plate, well, and chemical.
#'   \item Optionally applies a log10 transformation to AUC ratios.
#'   \item Generates a heatmap using \code{ggplot2::geom_tile()}.
#'   \item Facets the plot by plate type and additional experimental variables.
#' }
#'
#' @return A \code{ggplot} object representing the AUC ratio heatmap.
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
#' plot_auc_ratio_heatmap(processed_data, plate_type = "PM-M01", x_var = "treatment")
#'
#'
#' @export
plot_auc_ratio_heatmap <- function(data,
                                   plate_type,
                                   x_var = NULL,
                                   scale_type = c("natural", "log10")) {
  
  # ---- Validate top-level structure ----
  if (!is.list(data) || is.data.frame(data) ||
      !"auc_ratio" %in% names(data)) {
    stop("'data' must be a named list containing an 'auc_ratio' element,
          as produced by compute_auc_ratio().")
  }
  
  # ---- Validate auc_ratio ----
  data_df <- data$auc_ratio
  
  if (!is.data.frame(data_df)) {
    stop("'data$auc_ratio' must be a data.frame.")
  }
  
  if (nrow(data_df) == 0) {
    stop("'data$auc_ratio' must contain at least one row.")
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
  
  # ---- Validate required columns in auc_ratio ----
  required_cols <- c("Plate.Type", "Well", "Chemical", "AUC_ratio")
  missing_cols  <- setdiff(required_cols, names(data_df))
  if (length(missing_cols) > 0) {
    stop(paste("'data$auc_ratio' is missing required columns:",
               paste(missing_cols, collapse = ", ")))
  }
  
  # ---- Validate plate_type ----
  if (missing(plate_type) || is.null(plate_type) || length(plate_type) == 0) {
    stop("'plate_type' must be a non-empty character vector.")
  }
  
  if (!is.character(plate_type)) {
    stop("'plate_type' must be a character vector.")
  }
  
  unknown_plates <- setdiff(plate_type, unique(data_df$Plate.Type))
  if (length(unknown_plates) > 0) {
    warning(paste("The following 'plate_type' values were not found in",
                  "'data$auc_ratio$Plate.Type':",
                  paste(unknown_plates, collapse = ", ")))
  }
  
  # ---- Validate scale_type ----
  scale_type <- match.arg(scale_type)
  
  # ---- Auto-select x_var if NULL ----
  if (is.null(x_var)) {
    if (is.null(data$meta$variables) || length(data$meta$variables) == 0) {
      stop("No 'variables' found in 'data$meta'. Cannot auto-select 'x_var'.",
           " Please provide a valid 'x_var'.") 
    }
    
    # Use the first candidate
    x_var <- data$meta$variables[1]
    message("Using '", x_var, "' as default 'x_var' (first entry 'data$meta$variables').")
  }
  
  if (!is.character(x_var) || length(x_var) != 1) {
    stop("'x_var' must be a single character string.")
  }
  
  if (!x_var %in% names(data_df)) {
    stop(paste0("'x_var' value '", x_var,
                "' is not a column in 'data$auc_ratio'."))
  }
  
  # ---- Post-filter check ----
  data_filtered <- data_df %>%
    dplyr::filter(.data$"Plate.Type" %in% plate_type)
  
  if (nrow(data_filtered) == 0) {
    stop("No data found for the specified 'plate_type' values.")
  }
  
  # ---- Prepare variables ----
  
  variables    <- data$meta$variables
  column_vars <- setdiff(variables, x_var)
  
  data_filtered <- data_df %>%
    dplyr::filter(.data$"Plate.Type" %in% plate_type) %>%
    dplyr::mutate(
      ID = paste0(.data$"Plate.Type", "_", .data$Well, "_", .data$Chemical)
    )
  
  # ---- Check if x_var has only one unique value ----
  unique_x <- unique(data_filtered[[x_var]])
  n_unique_x <- length(unique_x)
  
  if (n_unique_x == 1) {
    warning("The variable '", x_var, "' has only one unique value (",
            unique_x, ").",
            "A heatmap cannot show variation along this axis.
            Consider selecting a different x_var or checking your data.")
  }
  
  
  
  # ---- Apply scale transformation ----
  if (scale_type == "log10") {
    data_filtered <- data_filtered %>%
      dplyr::mutate(AUC_ratio_plot = log10(.data$AUC_ratio))
    midpoint_value <- 0
    legend_title <- expression(log[10]~"(AUC ratio)")
  } else {
    data_filtered <- data_filtered %>%
      dplyr::mutate(AUC_ratio_plot = .data$AUC_ratio)
    midpoint_value <- 1
    legend_title <- "AUC ratio"
  }
  
  # ---- Reverse ID order ----
  id_levels <- data_filtered %>%
    dplyr::distinct(.data$ID) %>%
    dplyr::pull(.data$ID)
  
  data_filtered <- data_filtered %>%
    dplyr::mutate(
      ID = factor(.data$ID, levels = rev(id_levels))
    )
  
  # ---- Facet formula ----
  if (length(column_vars) > 0) {
    facet_formula <- as.formula(
      paste("Plate.Type ~", paste(column_vars, collapse = " + "))
    )
  } else {
    facet_formula <- as.formula("Plate.Type ~ .")
  }
  
  # ---- Plot ----
  ggplot2::ggplot(
    data_filtered,
    ggplot2::aes(
      x = .data[[x_var]],
      y = .data$ID,
      fill = .data$AUC_ratio_plot
    )
  ) +
    ggplot2::geom_tile(color = "grey90", linewidth = 0.2) +
    ggplot2::facet_grid(
      facet_formula,
      scales = "free_y",
      space  = "free_y",
      labeller = ggplot2::label_wrap_gen(width = 20)
    ) +
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
      title = paste0("AUC Ratio Heatmap (", scale_type, " scale)"),
      subtitle = paste0(
        "Plate.Type: ", paste(plate_type, collapse = ", "),
        "\nAUC ratio = AUC[well] / mean(AUC[control on plate])",
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
