#' Quality Control for Replicate Consistency
#'
#' Performs quality control on replicate measurements by:
#' \itemize{
#'   \item Filtering selected wells
#'   \item Detecting duplicate entries
#'   \item Summarising replicate counts
#'   \item Generating a QC heatmap
#' }
#'
#' @param data A data frame containing plate-based measurements.
#' @param wells Character vector of wells to inspect.
#' @param group_vars Character vector of grouping variables.
#'
#' @importFrom dplyr filter
#' @importFrom dplyr count
#' @importFrom dplyr across
#' @importFrom dplyr all_of
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @import ggplot2
#' @import magrittr
#' @return A named list with QC outputs:
#' \describe{
#'   \item{qc_data}{Filtered dataset}
#'   \item{duplicates}{Detected duplicate rows}
#'   \item{qc_table}{Replicate summary table}
#'   \item{qc_plot}{Heatmap visualization}
#' }
#' @export
qc_replicate <- function(
    data,
    wells = c("A01","A02","A03","A04"),
    group_vars = c("cell_line", "Plate.Type", "Description", "Well", "Hour")
) {
  
  # ---- Input validation ----
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame.")
  }
  
  if (!is.character(wells) || length(wells) == 0) {
    stop("'wells' must be a non-empty character vector.")
  }
  
  if (!is.character(group_vars) || length(group_vars) == 0) {
    stop("'group_vars' must be a non-empty character vector.")
  }
  
  missing_cols <- setdiff(group_vars, names(data))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # ---- Filter wells ----
  qc_data <- dplyr::filter(data, .data$Well %in% wells)
  
  if (nrow(qc_data) == 0) {
    stop("No data found for the specified wells.")
  }
  
  # ---- Duplicate detection ----
  duplicates <- qc_data %>%
    dplyr::count(dplyr::across(dplyr::all_of(group_vars))) %>%
    dplyr::filter(.data$n > 1)
  
  # ---- Replicate summary ----
  qc_table <- qc_data %>%
    dplyr::count(dplyr::across(dplyr::all_of(group_vars))) %>%
    dplyr::group_by(
      dplyr::across(dplyr::all_of(setdiff(group_vars, "Hour")))
    ) %>%
    dplyr::summarise(
      n_entries = max(.data$n),
      .groups = "drop"
    )
  
  # ---- QC plot ----
  qc_plot <- ggplot2::ggplot(
    qc_table,
    ggplot2::aes(
      x = .data$Well,
      y = .data$Description,
      fill = .data$n_entries
    )
  ) +
    ggplot2::geom_tile(color = "grey70") +
    ggplot2::facet_wrap(~Plate.Type, scales = "free_x") +
    ggplot2::scale_fill_gradient(name = "Replicates") +
    ggplot2::labs(
      title = "Replicate QC Heatmap",
      subtitle = paste("Wells:", paste(wells, collapse = ", ")),
      x = "Well",
      y = "Description"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
  
  # ---- QC flag summary  ----
  qc_flags <- dplyr::mutate(
    qc_table,
    qc_status = dplyr::case_when(
      .data$n_entries == 1 ~ "OK",
      .data$n_entries > 1 ~ "Duplicate",
      TRUE ~ "Unknown"
    )
  )
  
  return(list(
    qc_data = qc_data,
    duplicates = duplicates,
    qc_table = qc_table,
    qc_flags = qc_flags,
    qc_plot = qc_plot
  ))
}