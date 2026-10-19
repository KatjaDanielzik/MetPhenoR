#' Exclude 96-well plate
#'
#' Excludes one 96-well plate specified by plate_type and filters from data$data.
#'
#' @param data A structured list containing:
#'   \describe{
#'     \item{data}{A data frame of raw OD time-series measurements.}
#'     \item{meta}{Metadata including grouping variables.}
#'     \item{classification}{Well-level annotations (must include
#'     \code{well_summary}).}
#'   }
#' @param plate_type Character vector of Plate.Type to exclude in combination with filters
#' @param filters Named list of filtering conditions that should be excluded in plates with 'plate_type'.
#' E.g. list(radiation.dose="0Gy",time.h="2h"),
#' were list elements have to be the user provided 'variables' [read_folder()]
#'
#' @return Filtered data$data element.
#'
#' @import magrittr
#' @importFrom dplyr filter
#' @importFrom rlang .data
#'
#' @examples
#' data("processed_data")
#' # before filtering
#' qc_data(processed_data)
#' processed_data <- filter_data(processed_data,
#'   plate_type = "PM-M01",
#'   filters = list(
#'     cell.line = "A",
#'     treatment = "treatment", inhibitor = "inhibitor", time = "2h"
#'   )
#' )
#' # after filtering
#' qc_data(processed_data)
#'
#' @export
filter_data <- function(data, plate_type, filters = NULL) {
  # ---- Validate top-level data structure ----
  if (!is.list(data) || !all(c("data", "meta") %in% names(data))) {
    stop("'data' must be a named list with elements 'data' and 'meta',
          as produced by read_folder().")
  }

  if (!is.list(data$meta) || !"variables" %in% names(data$meta)) {
    stop("'data$meta' must be a named list containing 'variables',
          as produced by read_folder().")
  }

  # ---- Extract ----
  df <- data$data
  variables <- data$meta$variables

  # ---- Validate data$data ----
  if (!is.data.frame(df)) {
    stop("'data$data' must be a data.frame.")
  }

  if (nrow(df) == 0) {
    stop("'data$data' must contain at least one row.")
  }

  # ---- Validate variables ----
  if (!is.character(variables) || length(variables) == 0) {
    stop("'data$meta$variables' must be a non-empty character vector.")
  }

  # ---- Validate Plate.Type column ----
  if (!"Plate.Type" %in% names(df)) {
    stop("'Plate.Type' column not found in data$data.")
  }

  # ---- Validate plate_type argument ----
  if (missing(plate_type) || is.null(plate_type) || length(plate_type) == 0) {
    stop("'plate_type' must be a non-empty character vector.")
  }

  if (!is.character(plate_type)) {
    stop("'plate_type' must be a character vector.")
  }

  # ---- Warn if plate_type not found in data ----
  unknown_plates <- setdiff(plate_type, unique(df$Plate.Type))
  if (length(unknown_plates) > 0) {
    warning(paste(
      "The following 'plate_type' values were not found in 'data$data$Plate.Type' and will have no effect:",
      paste(unknown_plates, collapse = ", ")
    ))
  }

  # ---- Validate filters ----
  if (!is.null(filters)) {
    if (!is.list(filters)) {
      stop("'filters' must be a named list or NULL.")
    }

    if (length(filters) == 0) {
      stop("'filters' must contain at least one element when not NULL.")
    }

    if (is.null(names(filters)) || any(names(filters) == "")) {
      stop("All elements of 'filters' must be named.")
    }

    unknown_vars <- setdiff(names(filters), variables)
    if (length(unknown_vars) > 0) {
      stop(paste(
        "The following filter names are not in 'data$meta$variables':",
        paste(unknown_vars, collapse = ", ")
      ))
    }

    if (!all(sapply(filters, function(x) is.character(x) && length(x) == 1))) {
      stop("Each element of 'filters' must be a single character string.")
    }
  }

  # Filtering function
  filter_dataframe_excluded <- function(df, plate, conditions_list) {
    # Create the condition for plate_type
    plate_condition <- df$Plate.Type == plate

    # Create logical vectors for each condition in the list
    condition_vectors <- lapply(names(conditions_list), function(col_name) {
      df[[col_name]] == conditions_list[[col_name]]
    })

    # Combine all condition vectors with AND
    all_conditions <- Reduce(`&`, condition_vectors)

    # Combine plate_type condition with all other conditions
    matching_condition <- plate_condition & all_conditions

    # Return rows that DO NOT match (exclude the matching ones)
    return(df[!matching_condition, ])
  }

  filtered_data <- filter_dataframe_excluded(df, plate_type, filters)

  data$data <- filtered_data
  return(data)
}
