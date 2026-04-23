#' Filter Combined Experimental Data
#'
#' Filters a dataset by Plate.Type and additional user-defined conditions.
#'
#' @param data A data frame.
#' @param plate_type Character vector of Plate Type values to keep.
#' @param filters Named list of filtering conditions.
#'
#' @return Filtered data frame.
#' 
#' @import magrittr
#' @importFrom dplyr filter
#' @importFrom rlang .data
#' 
#' @examples
#' data("processed_data")
#' processed_data <- filter_data(processed_data, plate_type = "PM-M01")
#' 
#' @export
filter_data <- function(data, plate_type, filters = list()) {
  
  data <- data$data
  
  # ---- Input validation ----
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame.")
  }
  
  if (missing(plate_type) || length(plate_type) == 0) {
    stop("'plate_type' must be a non-empty character vector.")
  }
  
  if (!"Plate Type" %in% names(data)) {
    stop("'Plate Type' column not found in data.")
  }
  
  if (!is.list(filters)) {
    stop("'filters' must be a named list.")
  }
  
  # Filter by Plate Type ----
  filtered_data <- dplyr::filter(
    data,
    .data$`Plate Type` %in% plate_type
  )
  
  #Apply dynamic filters ----
  if (length(filters) > 0) {
    
    for (var_name in names(filters)) {
      
      if (!var_name %in% names(filtered_data)) {
        stop(paste("Column", var_name, "does not exist in data."))
      }
      
      values <- filters[[var_name]]
      
      filtered_data <- dplyr::filter(
        filtered_data,
        .data[[var_name]] %in% values
      )
    }
  }
  
  return(filtered_data)
}