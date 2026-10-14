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
#' @param plate_type Character vector of Plate Type to exclude in combination with filters
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
#' #before filtering
#' qc_data(processed_data)
#' processed_data <- filter_data(processed_data, plate_type = "PM-M01",
#' filters = list(cell.line="A",
#' treatment="treatment",inhibitor="inhibitor",time="2h"))
#' # after filtering
#' qc_data(processed_data)
#' 
#' @export
filter_data <- function(data, plate_type, filters = NULL){
  
  variables <- data$meta$variables
  df <- data$data
  
  # ---- Input validation ----
  if (!is.data.frame(df)) {
    stop("'data' must contain a dataframe called 'data'")
  }
  
  if (missing(plate_type) || length(plate_type) == 0) {
    stop("'plate_type' must be a non-empty character vector.")
  }
  
  if (!"Plate Type" %in% names(df)) {
    stop("'Plate Type' column not found in data$data.")
  }
  
  if (!is.list(filters)) {
    stop("'filters' must be a named list")
  }
  
  if (any(!names(filters) %in% variables)) {
    missing_elements <- setdiff(names(filters), variables)
    stop("The following variables are not specified in 'filters': ", 
         paste(missing_elements, collapse = ", "))
  }
  
  # Filtering function
  filter_dataframe_excluded <- function(df, plate, conditions_list) {
    # Create the condition for plate_type
    plate_condition <- df$`Plate Type` == plate
    
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
  
  filtered_data <- filter_dataframe_excluded(df,plate_type,filters)
  
  data$data <- filtered_data
  return(data)
}