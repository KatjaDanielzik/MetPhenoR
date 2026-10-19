#' Plot Final Time Point per Experimental Condition
#'
#' This function extracts the final (latest) time point for each well across
#' experimental conditions and visualizes the corresponding measurements in a
#' 96-well plate layout.
#'
#' Each plot represents a single experimental condition and displays wells as
#' spatially arranged points, colored according to their measured OD at the
#' final time point.
#'
#' @param data A named list produced by \code{read_folder()}, containing:
#' \describe{
#'   \item{data}{A \code{data.frame} with experimental measurements}
#'   \item{meta}{A list containing metadata, including a character vector
#'   \code{variables} specifying grouping variables}
#' }
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Groups the data by experimental variables, Plate.Type, and Well.
#'   \item Extracts the latest time point per well.
#'   \item Converts well identifiers (e.g., "A01") into plate coordinates.
#'   \item Generates a separate plate-layout plot for each experimental condition.
#' }
#'
#' Color intensity reflects the magnitude of \code{diff_OD_590_740} at the final
#' recorded time point.
#'
#' @return A named list of \code{ggplot2} objects, where each element corresponds
#'   to a unique experimental condition.
#'
#' @import dplyr
#' @import ggplot2
#' @import magrittr
#' @importFrom tidyr unite
#' @importFrom stringr str_extract
#' @importFrom purrr map_chr map
#' @importFrom tidyselect all_of
#' @importFrom rlang .data
#'
#' @examples
#' data("processed_data")
#' plot_latest_timepoint(processed_data)
#' 
#' 
#' @export
plot_latest_timepoint <- function(data) {
  
  # Check top-level structure
  if (!is.list(data) || !all(c("data", "meta") %in% names(data))) {
    stop("'data' must be a named list with elements 'data' and 'meta',
          as produced by read_folder().")
  }
  
  if (!is.list(data$meta) || !"variables" %in% names(data$meta)) {
    stop("'data$meta' must be a named list containing 'variables',
          as produced by read_folder().")
  }
  
  data_df   <- data$data
  variables <- data$meta$variables
  
  if (!is.data.frame(data_df)) {
    stop("'data$data' must be a data.frame.")
  }
  
  if (!is.character(variables) || length(variables) == 0) {
    stop("'data$meta$variables' must be a non-empty character vector.")
  }
  
  if (nrow(data_df) == 0) {
    stop("'data$data' must contain at least one row.")
  }
  
  required_cols <- c(variables, "Plate.Type", "Well", "Hour", "diff_OD_590_740")
  missing_cols  <- setdiff(required_cols, names(data_df))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  #Extract latest time point per well
  latest_timepoint <- data_df %>%
    dplyr::group_by(
      dplyr::across(dplyr::all_of(c(variables, "Plate.Type", "Well")))
    ) %>%
    dplyr::slice_max(.data$Hour, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup()
  
  #Convert well identifiers to plate coordinates
  latest_timepoint <- latest_timepoint %>%
    dplyr::mutate(
      category = stringr::str_extract(.data$Well, "^[A-H]"),
      number = as.numeric(stringr::str_extract(.data$Well, "\\d+"))
    )
  
  #Create grouping identifier
  latest_timepoint <- latest_timepoint %>%
    tidyr::unite(
      "group_id",
      tidyselect::all_of(c(variables, "Plate.Type")),
      remove = FALSE,
      sep = "_"
    )
  
  #Split data and generate plots
  split_data <- dplyr::group_split(latest_timepoint, .data$group_id)
  
  plot_names <- latest_timepoint %>%
    dplyr::distinct(.data$group_id) %>%
    dplyr::pull(.data$group_id)
  
  latest_timepoint_plots <- purrr::map(split_data, .plot_latest_point,variables)
  names(latest_timepoint_plots) <- plot_names
  
  return(latest_timepoint_plots)
}
