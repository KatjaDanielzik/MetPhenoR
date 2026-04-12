#' Annotate assay data with chemical
#'
#' Integrates experimental assay data with one or more annotation data frames
#' to assign chemical identities to wells. This version assumes that annotation
#' data are already loaded into the R environment.
#'
#' @param data A list-like object containing a data frame element \code{data},
#'   typically produced by \code{read_folder()}. The data frame must include
#'   columns \code{"Plate Type"} and \code{"Well"}.
#' @param annotation A data frame or a list of data frames containing annotation
#'   metadata. Each must include columns \code{"Plate Type"} and \code{"Well"}.
#' @param control_map Optional named list defining control classifications.
#'   Each name corresponds to a control class and each element is a character
#'   vector of matching chemical names.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Accepts one or multiple annotation data frames.
#'   \item Combines annotation tables and removes duplicate rows.
#'   \item Merges annotation metadata into the experimental dataset.
#'   \item assigns control categories based on chemical identity.
#' }
#'
#' Column name matching is case-insensitive and whitespace-tolerant.
#'
#' @return The input \code{data} object with an updated \code{data} element,
#'   including annotation columns and an optional \code{control} column.
#'
#' @importFrom dplyr bind_rows distinct left_join mutate case_when select
#' @importFrom purrr imap
#' @importFrom rlang expr
#'
#' @export
annotate_chemical <- function(
    data,
    annotation,
    control_map = list( negative = c("Negative Control", "No Chemical"), 
                        positive = c("a-D-Glucose", "D-(+)-Glucose") ) ) {
  
  # ---- Validate input structure ----
  if (!("data" %in% names(data))) {
    stop("'data' must contain a 'data' element.")
  }
  
  data_df <- data$data
  
  if (!is.data.frame(data_df)) {
    stop("'data$data' must be a data.frame.")
  }
  
  # ---- Handle annotation input ----
  if (is.data.frame(annotation)) {
    annotations_list <- list(annotation)
  } else if (is.list(annotation) && all(purrr::map_lgl(annotation, is.data.frame))) {
    annotations_list <- annotation
  } else {
    stop("'annotation' must be a data.frame or a list of data.frames.")
  }
  
  # ---- Combine annotations ----
  annotations <- dplyr::bind_rows(annotations_list) %>%
    dplyr::distinct()
  
  # ---- Required columns ----
  required_cols <- c("Plate Type", "Well")
  
  if (!all(required_cols %in% names(annotations))) {
    stop(
      "Annotation must contain columns: ",
      paste(required_cols, collapse = ", "),
      "\nFound: ",
      paste(names(annotations), collapse = ", ")
    )
  }
  
  # ---- Merge ----
  data_df <- dplyr::left_join(
    data_df,
    dplyr::select(annotations, "Plate Type", "Well", "Chemical"),
    by = c("Plate Type", "Well")
  )
  
  # ---- Control classification ----
  if (!is.null(control_map) && "Chemical" %in% names(data_df)) {
    
    data_df <- dplyr::mutate(
      data_df,
      control = dplyr::case_when(
        !!!purrr::imap(control_map, ~ rlang::expr(Chemical %in% !!.x ~ !!.y)),
        TRUE ~ NA_character_
      )
    )
    
  }
  
  # ---- Return ----
  data$data <- data_df
  return(data)
}