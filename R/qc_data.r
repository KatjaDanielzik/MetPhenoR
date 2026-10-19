#' Quality Control for input data after noise correction
#' 
#' The function filters the first five wells,evaluates replicate consistency 
#' across experimental conditions, detects duplicated entries, and visualizes 
#' replicate counts using a heatmap.
#'
#' The QC workflow comprises:
#' \enumerate{
#'   \item Subsetting the dataset to first five wells
#'   \item Identifying duplicated measurements across grouping variables
#'   \item Summarising replicate counts per condition
#'   \item Visualizing replicate distribution as a heatmap
#' }
#'
#' @param data A named list produced by \code{read_folder()}, containing:
#' \describe{
#'   \item{data}{A \code{data.frame} with experimental measurements}
#'   \item{meta}{A list containing metadata, including a character vector
#'   \code{variables} a character vector specifying grouping variables}
#' }
#'
#' @details
#' The function internally restricts analysis to a fixed set of wells
#' (\code{A01–A05}). Replicate counts are computed across grouping variables
#' defined by \code{data$meta$variables}, in addition to \code{"Plate.Type"},
#' \code{"Hour"}, and \code{"Well"}.
#'
#' Duplicate entries are defined as observations sharing identical values across
#' all grouping variables and occurring more than once.
#'
#' The resulting heatmap displays the number of replicate measurements per
#' condition, facilitating rapid identification of inconsistencies such as
#' duplicated or missing replicates.
#'
#' @return A \code{ggplot2} object representing a heatmap of replicate counts
#' across wells and experimental conditions.
#' 
#' @import dplyr
#' @import ggplot2
#' @importFrom rlang .data
#'
#' @seealso \code{\link{read_folder}}
#'
#' @examples
#' data("processed_data")
#' qc_data(processed_data)
#'
#' @export
qc_data <- function(data) {
  
  # Check top-level input structure
  if (!is.list(data) || !all(c("data", "meta") %in% names(data))) {
    stop("'data' must be a named list with elements 'data' and 'meta', as produced by 'read_folder()'")
  }
  
  if (!is.list(data$meta) || !"variables" %in% names(data$meta)) {
    stop("'data$meta' must be a named list containing 'variables', as produced by read_folder().")
  }
  
  # Extract
  data_df   <- data$data
  variables <- data$meta$variables
  
  if (!is.data.frame(data_df)) {
    stop("'data$data' must be a data frame.")
  }
  
  if (!is.character(variables) || length(variables) == 0) {
    stop("'data$meta$variables' must be a non-empty character vector.")
  }
  
  # Derived internals
  wells      <- c("A01", "A02", "A03", "A04", "A05")
  group_vars <- c(variables, "Hour", "Well")
  
  # Column existence
  required_cols <- c(group_vars, "Plate.Type", "diff_OD_590_740")
  missing_cols  <- setdiff(required_cols, names(data_df))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # Post-filter check
  qc_data <- dplyr::filter(data_df, .data$Well %in% wells)
  if (nrow(qc_data) == 0) {
    stop("No data found for the specified wells.")
  }
  
  # Duplicate detection
  duplicates <- qc_data %>%
    dplyr::count(dplyr::across(dplyr::all_of(c(group_vars, "Plate.Type"))), name = "n") %>%
    dplyr::filter(.data$n > 1)
  
  # Replicate summary
  qc_table <- qc_data %>%
    dplyr::count(
      dplyr::across(dplyr::all_of(c(group_vars, "Plate.Type"))),
      name = "n"
    ) %>%
    dplyr::group_by(
      dplyr::across(dplyr::all_of(setdiff(c(group_vars, "Plate.Type"), "Hour")))
    ) %>%
    dplyr::summarise(
      n_entries = max(.data$n),
      Plate_Type = dplyr::first(.data$Plate.Type),
      .groups = "drop"
    )
  
  # QC flags
  qc_flags <- qc_table %>%
    dplyr::mutate(
      qc_status = dplyr::case_when(
        .data$n_entries == 1 ~ "OK",
        .data$n_entries > 1 ~ "Duplicate",
        TRUE ~ "Unknown"
      )
    )
  
  # Prepare plotting column
  qc_table <- qc_table %>%
    tidyr::unite(
      ".qc_y",
      dplyr::all_of(setdiff(group_vars, c("Well", "Hour"))),
      remove = FALSE
    )
  
  # QC plot
  qc_plot <- ggplot2::ggplot(
    qc_table,
    ggplot2::aes(
      x = .data$Well,
      y = .data$.qc_y,
      fill = .data$n_entries
    )
  ) +
    ggplot2::geom_tile(color = "grey70") +
    ggplot2::facet_wrap(~Plate_Type, scales = "free_x") +
    ggplot2::scale_fill_gradient(name = "n observations") +
    ggplot2::labs(
      title = "QC heatmap: Number of observations = 1?",
      subtitle = "raw_value entries of the first 5 wells",
      x = "Well",
      y = "Condition"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
  
  return(qc_plot)
}