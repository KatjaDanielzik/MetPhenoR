#' Annotate assay data with metadata
#'
#' Integrates experimental assay data with one or more annotation tables
#' to assign chemical or treatment identities to wells.
#'
#' @param data A list containing a data frame element \code{auc_ratio},
#'   typically produced by \code{read_folder()}. The data frame must include
#'   columns \code{"Plate Type"}, \code{"Well"}, and \code{"Chemical"}.
#' @param annotation A data frame or a list of data frames containing annotation
#'   metadata. Each must include columns \code{"Plate Type"}, \code{"Well"}, 
#'   and \code{"Chemical"}.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#' \item Accepts one or multiple annotation data frames.
#'   \item Combines annotation tables and removes duplicate rows.
#'   \item Merges annotation metadata into the experimental dataset
#'         using \code{"Chemical"} as keys.
#' }
#'
#' @return The input \code{data} object with an updated \code{auc_ratio}
#'   element that includes annotation columns.
#'
#' @importFrom dplyr bind_rows distinct left_join rename_with select
#' @importFrom purrr map_lgl
#' @importFrom rlang .data
#' 
#' @examples
#' data("processed_data")
#' processed_data <- annotate_chemical(processed_data, annotation = PM_M01_M02)
#' processed_data <- classify_growth_curve_types(processed_data)
#' processed_data <- compute_auc_ci(processed_data)
#' processed_data <- compute_auc_ratio(processed_data)
#' processed_data <- annotate_metadata(processed_data, annotation = PM_M01_M02)
#'
#' @export
annotate_metadata <- function(data, annotation) {
  
  #Validate input structure
  if (!("auc_ratio" %in% names(data))) {
    stop("'data' must contain an 'auc_ratio' element.")
  }
  
  data_df <- data$auc_ratio
  
  if (!is.data.frame(data_df)) {
    stop("'data$auc_ratio' must be a data.frame.")
  }
  
  #Handle annotation input
  if (is.data.frame(annotation)) {
    annotations_list <- list(annotation)
  } else if (is.list(annotation) && all(purrr::map_lgl(annotation, is.data.frame))) {
    annotations_list <- annotation
  } else {
    stop("'annotation' must be a data.frame or a list of data.frames.")
  }
  
  #Combine annotations
  annotations <- dplyr::bind_rows(annotations_list) %>%
    dplyr::distinct()
  
  #Required columns
  data_df <- data_df %>%
    dplyr::select(-.data[["Plate Type"]], -.data[["Well"]]) %>%
    dplyr::distinct()
  
  required_cols <- c("Chemical")
  
  if (!all(required_cols %in% names(annotations))) {
    stop(
      "Annotation must contain columns: ",
      paste(required_cols, collapse = ", ")
    )
  }
  
  if (!all(required_cols %in% names(data_df))) {
    stop(
      "Data must contain columns: ",
      paste(required_cols, collapse = ", ")
    )
  }
  
  #Merge
  data_df <- dplyr::left_join(
    data_df,
    annotations,
    by = required_cols
  )
  
  #Return
  data$auc_ratio <- data_df
  return(data)
}

