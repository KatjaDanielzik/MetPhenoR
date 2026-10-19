#' Visualize Growth Curves and Plate Layouts from Classified Data
#'
#' Generates combined visualizations of growth curve classifications and
#' corresponding raw optical density (OD) time-series data. For each experimental
#' condition, a plate layout heatmap is displayed alongside faceted growth curves,
#' enabling direct comparison between spatial well distribution and temporal
#' growth dynamics.
#'
#' @param data A structured list produced by upstream processing steps, containing:
#'   \describe{
#'     \item{data}{A data frame of raw OD time-series measurements.}
#'     \item{meta}{A list containing metadata, including a character vector
#'     \code{variables} defining grouping variables (e.g., strain, condition).}
#'     \item{classification}{A list containing outputs from
#'     \code{\link{classify_growth_curve_types}}, including:
#'       \describe{
#'         \item{well_summary}{Per-well classification results.}
#'         \item{derivative_data}{Spline and derivative data (not directly plotted).}
#'       }
#'     }
#'   }
#' @param curve_types_to_plot Optional character vector specifying which
#'   curve types to include in the raw growth curve visualization.
#'   If \code{NULL} (default), all detected curve types except \code{"growing"}
#'   are included.
#' @param store_plots Logical. If \code{TRUE} (default), generated plots are stored
#'   within \code{data$visualization$plate_plots} and the modified object is returned.
#'   If \code{FALSE}, only the list of plots is returned.
#'
#' @details
#' For each unique combination of grouping variables and plate type, the function:
#' \enumerate{
#'   \item Constructs a plate layout heatmap (8×12 format) showing well-wise
#'   growth classifications.
#'   \item Overlays raw OD time-series curves, grouped by well and faceted by
#'   curve type.
#'   \item Combines both visualizations using \pkg{patchwork} for side-by-side
#'   comparison.
#' }
#'
#' Missing wells in the plate layout are labeled as \code{"empty"}.
#' If no curves match the selected \code{curve_types_to_plot}, an empty panel
#' is displayed in place of the time-series plot.
#'
#' @return
#' If \code{store_plots = TRUE}, returns the input \code{data} object augmented with:
#' \describe{
#'   \item{visualization$plate_plots}{A named list of combined \pkg{ggplot2}
#'   objects, one per experimental condition.}
#' }
#'
#' If \code{store_plots = FALSE}, returns a named list of \pkg{ggplot2} objects.
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom stats na.omit
#' @importFrom rlang .data
#'
#' @seealso \code{\link{classify_growth_curve_types}}
#'
#' @examples
#' data("processed_data")
#' processed_data <- classify_growth_curve_types(processed_data)
#' processed_data <- plot_growth_curve(processed_data)
#' print(processed_data[["growth_curve_plots"]])
#'
#' @export

plot_growth_curve <- function(
    data,
    curve_types_to_plot = NULL,
    store_plots = TRUE
) {
  
  # ---- Validate top-level data structure ----
  if (!is.list(data) || !all(c("data", "meta", "classification") %in% names(data))) {
    stop("'data' must be a named list with elements 'data', 'meta', and 'classification', as produced by classify_growth_curve_types().")
  }
  
  if (!is.list(data$meta) || !"variables" %in% names(data$meta)) {
    stop("'data$meta' must be a named list containing 'variables', as produced by read_folder().")
  }
  
  if (!is.list(data$classification) ||
      !all(c("well_summary", "derivative_data") %in% names(data$classification))) {
    stop("'data$classification' must be a named list with elements 'well_summary' and 'derivative_data'. Run classify_growth_curve_types() first.")
  }
  
  # ---- Extract ----
  raw_data     <- data$data
  variables    <- data$meta$variables
  well_summary <- data$classification$well_summary
  
  # ---- Validate data$data ----
  if (!is.data.frame(raw_data)) {
    stop("'data$data' must be a data.frame.")
  }
  
  if (nrow(raw_data) == 0) {
    stop("'data$data' must contain at least one row.")
  }
  
  # ---- Validate variables ----
  if (!is.character(variables) || length(variables) == 0) {
    stop("'data$meta$variables' must be a non-empty character vector.")
  }
  
  # ---- Validate required columns in data$data ----
  required_data_cols <- c(variables, "Plate.Type", "Well", "Hour", "diff_OD_590_740")
  missing_data_cols  <- setdiff(required_data_cols, names(raw_data))
  if (length(missing_data_cols) > 0) {
    stop(paste("'data$data' is missing required columns:",
               paste(missing_data_cols, collapse = ", ")))
  }
  
  # ---- Validate required columns in well_summary ----
  required_ws_cols <- c(variables, "Plate.Type", "Well", "curve_type")
  missing_ws_cols  <- setdiff(required_ws_cols, names(well_summary))
  if (length(missing_ws_cols) > 0) {
    stop(paste("'data$classification$well_summary' is missing required columns:",
               paste(missing_ws_cols, collapse = ", ")))
  }
  
  # ---- Validate curve_types_to_plot ----
  if (!is.null(curve_types_to_plot)) {
    if (!is.character(curve_types_to_plot) || length(curve_types_to_plot) == 0) {
      stop("'curve_types_to_plot' must be a non-empty character vector or NULL.")
    }
    valid_types <- c("growing", "flat_low", "OD_drop", "slow_growing", "uncertain")
    unknown_types <- setdiff(curve_types_to_plot, valid_types)
    if (length(unknown_types) > 0) {
      warning(paste("'curve_types_to_plot' contains unrecognised curve types:",
                    paste(unknown_types, collapse = ", "),
                    "\nValid types are:", paste(valid_types, collapse = ", ")))
    }
  }
  
  # ---- Validate store_plots ----
  if (!is.logical(store_plots) || length(store_plots) != 1) {
    stop("'store_plots' must be a single logical value (TRUE or FALSE).")
  }
  
  # Shared color palette
  curve_colors <- c(
    "OD_drop"      = "red",
    "flat_low"     = "blue",
    "growing"      = "darkgreen",
    "slow_growing" = "orange"
  )
  
  curve_levels <- names(curve_colors)
  
  # Extract data
  join_keys <- c(variables, "Plate.Type")
  
  conditions <- well_summary %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(join_keys)))
  
  plot_list <- vector("list", nrow(conditions))
  names(plot_list) <- apply(conditions, 1, paste, collapse = "_")
  
  # Loop over conditions
  for (i in seq_len(nrow(conditions))) {
    
    cond <- conditions[i, ]
    
    ws <- well_summary %>%
      dplyr::semi_join(cond, by = join_keys)
    
    # Plate layout
    plate_layout <- expand.grid(
      category = LETTERS[1:8],
      number   = 1:12
    )
    
    ws_plate <- ws %>%
      dplyr::mutate(
        category = stringr::str_extract(.data$Well, "^[A-H]"),
        number   = as.numeric(stringr::str_extract(.data$Well, "\\d+"))
      )
    
    plate_df <- plate_layout %>%
      dplyr::left_join(
        dplyr::select(ws_plate, dplyr::all_of(c("category", "number", "curve_type"))),
        by = c("category", "number")
      ) %>%
      dplyr::mutate(
        curve_type = tidyr::replace_na(.data$curve_type, "empty"),
        curve_type = factor(.data$curve_type, levels = curve_levels),
        category   = factor(.data$category, levels = LETTERS[8:1])
      )
    
    title_text <- paste(cond, collapse = " | ")
    
    p_plate <- ggplot2::ggplot(
      plate_df,
      ggplot2::aes(x = .data$number, y = .data$category)
    ) +
      ggplot2::geom_tile(
        ggplot2::aes(fill = .data$curve_type),
        color = "white"
      ) +
      ggplot2::coord_fixed() +
      ggplot2::scale_x_continuous(breaks = 1:12, expand = c(0, 0)) +
      ggplot2::scale_y_discrete(expand = c(0, 0)) +
      ggplot2::scale_fill_manual(
        values = curve_colors,
        drop = FALSE
      ) +
      ggplot2::labs(
        title = title_text,
        x = NULL,
        y = NULL,
        fill = "curve_type"
      ) +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(
        panel.grid = ggplot2::element_blank(),
        axis.text  = ggplot2::element_text(size = 8),
        legend.position = "bottom"
      )
    
    # Raw curve data
    raw_subset <- raw_data %>%
      dplyr::semi_join(cond, by = join_keys) %>%
      dplyr::left_join(
        dplyr::select(ws, dplyr::all_of(join_keys), .data$Well, .data$curve_type),
        by = c(join_keys, "Well")
      )
    
    # Determine which curve types to plot
    if (is.null(curve_types_to_plot)) {
      curve_types_current <- raw_subset %>%
        dplyr::pull(.data$curve_type) %>%
        unique() %>%
        stats::na.omit() %>%
        setdiff("growing")
    } else {
      curve_types_current <- curve_types_to_plot
    }
    
    raw_subset <- raw_subset %>%
      dplyr::filter(.data$curve_type %in% curve_types_current)
    
    # Handle empty case
    if (nrow(raw_subset) == 0) {
      
      p_raw <- ggplot2::ggplot() + ggplot2::theme_void()
      
      plot_list[[i]] <- patchwork::wrap_plots(
        p_plate, p_raw,
        widths = c(1, 1.7)
      )
      
      next
    }
    
    raw_subset <- raw_subset %>%
      dplyr::mutate(
        curve_type = factor(.data$curve_type, levels = curve_levels)
      )
    
    # Raw curve plot
    p_raw <- ggplot2::ggplot(
      raw_subset,
      ggplot2::aes(
        x = .data$Hour,
        y = .data$diff_OD_590_740,
        group = .data$Well,
        colour = .data$Well
      )
    ) +
      ggplot2::geom_line(linewidth = 0.8, alpha = 0.9) +
      ggplot2::facet_wrap(~curve_type, scales = "free_y", drop = TRUE) +
      ggplot2::labs(
        x = "Hour",
        y = "Raw OD",
        colour = "Well"
      ) +
      ggplot2::theme_bw(base_size = 13) +
      ggplot2::theme(
        legend.position = "right",
        strip.background = ggplot2::element_rect(fill = "grey90"),
        strip.text = ggplot2::element_text(face = "bold"),
        legend.key.height = grid::unit(0.4, "cm"),
        legend.text = ggplot2::element_text(size = 6)
      )
    
    # Combine plots
    plot_list[[i]] <- patchwork::wrap_plots(
      p_plate, p_raw,
      widths = c(1, 1.7)
    )
  }
  
  # Return
  if (store_plots) {
    data$growth_curve_plots <- plot_list
    return(data)
  } else {
    return(plot_list)
  }
}