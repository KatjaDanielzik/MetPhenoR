#' Plot 96-Well Plate Layout with Raw Growth Curves
#'
#' This function visualizes experimental results from 96-well plate assays by
#' combining (i) a spatial representation of the plate layout and (ii) the
#' corresponding raw time-series growth curves. For each unique experimental
#' condition, a composite plot is generated.
#'
#' The plate layout highlights the classification of each well (e.g., growth
#' behavior), while the accompanying panel displays the raw optical density
#' trajectories over time.
#'
#' @param well_summary A data frame containing per-well classifications,
#'   including curve types and experimental conditions.
#' @param raw_data A data frame containing raw time-series measurements
#'   (e.g., optical density values over time).
#' @param variables A character vector specifying the column names that define
#'   experimental conditions (e.g., treatment, strain).
#' @param curve_types_to_plot Optional character vector specifying which curve
#'   types should be included in the raw data visualization. If `NULL`, all
#'   curve types except `"growing"` are displayed.
#'
#' @details
#' The function iterates over all unique combinations of the specified
#' experimental variables and generates a composite plot for each condition.
#'
#' Each composite consists of:
#' \itemize{
#'   \item A plate layout (8 × 12 grid) showing well classifications.
#'   \item A faceted line plot of raw growth curves grouped by curve type.
#' }
#'
#' Wells without assigned classifications are labeled as `"empty"`.
#'
#' @return A named list of `ggplot2` objects, where each element corresponds to
#'   a specific experimental condition. Each plot combines the plate layout and
#'   raw growth curves using the \pkg{patchwork} package.
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom stringr str_extract
#' @importFrom tidyr replace_na
#' @importFrom patchwork plot_layout
#'
#' @export
plot_96well_with_raw <- function(
    well_summary,
    raw_data,
    variables,
    curve_types_to_plot = NULL
) {
  
  # ---- Input validation ----
  required_ws <- c(variables, "Plate.Type", "Well", "curve_type")
  required_raw <- c(variables, "Plate.Type", "Well", "Hour", "diff_OD_590_740")
  
  missing_ws <- setdiff(required_ws, names(well_summary))
  missing_raw <- setdiff(required_raw, names(raw_data))
  
  if (length(missing_ws) > 0) {
    stop(paste("Missing columns in well_summary:", paste(missing_ws, collapse = ", ")))
  }
  
  if (length(missing_raw) > 0) {
    stop(paste("Missing columns in raw_data:", paste(missing_raw, collapse = ", ")))
  }
  
  join_keys <- c(variables, "Plate.Type")
  
  conditions <- well_summary %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(join_keys)))
  
  plot_list <- vector("list", nrow(conditions))
  
  for (i in seq_len(nrow(conditions))) {
    
    cond <- conditions[i, ]
    
    # ---- Subset data for current condition ----
    ws <- dplyr::semi_join(well_summary, cond, by = join_keys)
    
    # ==========================================================
    # 1. Plate layout construction
    # ==========================================================
    
    plate_layout <- expand.grid(
      category = LETTERS[1:8],
      number = 1:12
    )
    
    ws_plate <- ws %>%
      dplyr::mutate(
        category = stringr::str_extract(.data$Well, "^[A-H]"),
        number = as.numeric(stringr::str_extract(.data$Well, "\\d+"))
      )
    
    plate_df <- plate_layout %>%
      dplyr::left_join(
        dplyr::select(ws_plate, .data$category, .data$number, .data$curve_type),
        by = c("category", "number")
      ) %>%
      dplyr::mutate(
        curve_type = tidyr::replace_na(.data$curve_type, "empty"),
        category = factor(.data$category, levels = LETTERS[8:1])
      )
    
    p_plate <- ggplot2::ggplot(
      plate_df,
      ggplot2::aes(x = .data$number, y = .data$category)
    ) +
      ggplot2::geom_tile(ggplot2::aes(fill = .data$curve_type), color = "white") +
      ggplot2::coord_fixed() +
      ggplot2::scale_fill_manual(values = c(
        "OD_drop" = "red",
        "flat_low" = "blue",
        "growing" = "darkgreen",
        "slow_growing" = "orange",
        "uncertain" = "grey50",
        "empty" = "grey95"
      )) +
      ggplot2::theme_minimal()
    
    # ==========================================================
    # 2. Raw growth curves
    # ==========================================================
    
    raw_subset <- raw_data %>%
      dplyr::semi_join(cond, by = join_keys) %>%
      dplyr::left_join(
        dplyr::select(ws, dplyr::all_of(join_keys), .data$Well, .data$curve_type),
        by = c(join_keys, "Well")
      )
    
    if (is.null(curve_types_to_plot)) {
      curve_types_current <- setdiff(unique(raw_subset$curve_type), "growing")
    } else {
      curve_types_current <- curve_types_to_plot
    }
    
    raw_subset <- dplyr::filter(raw_subset, .data$curve_type %in% curve_types_current)
    
    if (nrow(raw_subset) == 0) {
      plot_list[[i]] <- p_plate
      next
    }
    
    p_raw <- ggplot2::ggplot(
      raw_subset,
      ggplot2::aes(
        x = .data$Hour,
        y = .data$diff_OD_590_740,
        group = .data$Well,
        colour = .data$Well
      )
    ) +
      ggplot2::geom_line() +
      ggplot2::facet_wrap(~curve_type, scales = "free_y") +
      ggplot2::theme_bw()
    
    # ==========================================================
    # 3. Combine plots
    # ==========================================================
    
    plot_list[[i]] <- p_plate + p_raw +
      patchwork::plot_layout(widths = c(1, 1.7))
  }
  
  return(plot_list)
}