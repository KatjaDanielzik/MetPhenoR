#' Plot AUC ratio heatmap
#'
#' Generates a heatmap visualization of AUC ratios across wells, chemicals,
#' and experimental conditions using the \code{auc_ratio} element of the input data.
#'
#' @param data A list containing a data frame element \code{auc_ratio}.
#'   The data frame must include columns \code{"Plate Type"}, \code{"Well"},
#'   \code{"Chemical"}, and \code{"AUC_ratio"}.
#' @param plate_type Character vector specifying one or more plate types to include.
#' @param x_var Character string indicating the column to be mapped to the x-axis.
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
#' plot_auc_ratio_heatmap(processed_data, plate_type = "PM-M01", x_var = "radiation.dose")
#'
#'
#' @export
plot_auc_ratio_heatmap <- function(data,
                                   plate_type,
                                   x_var,
                                   scale_type = c("natural", "log10")) {
  
  # ---- Validate input ----
  if (!("auc_ratio" %in% names(data))) {
    stop("'data' must contain an 'auc_ratio' element.")
  }
  
  data_df <- data$auc_ratio
  
  if (!is.data.frame(data_df)) {
    stop("'data$auc_ratio' must be a data.frame.")
  }
  
  scale_type <- match.arg(scale_type)
  
  # ---- Prepare variables ----
  
  variables    <- data$meta$variables
  column_vars <- setdiff(variables, x_var)
  
  data_filtered <- data_df %>%
    dplyr::filter(.data$"Plate Type" %in% plate_type) %>%
    dplyr::mutate(
      ID = paste0(.data$"Plate Type", "_", .data$Well, "_", .data$Chemical)
    )
  
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
      paste("`Plate Type` ~", paste(column_vars, collapse = " + "))
    )
  } else {
    facet_formula <- as.formula("`Plate Type` ~ .")
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
        "Plate Type: ", paste(plate_type, collapse = ", "),
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
