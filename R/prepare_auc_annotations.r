#' Prepare Annotated AUC Data with Experimental Metadata
#'
#' This function integrates AUC estimates with external annotation files and
#' augments the dataset with experimental metadata and control classifications.
#'
#' Multiple annotation files can be supplied and are automatically combined into
#' a unified annotation table. The resulting dataset links per-well AUC estimates
#' to experimental descriptors such as treatment conditions or control status.
#'
#' @param auc_results A data frame containing AUC estimates, typically the output
#'   of \code{compute_auc_ci()}.
#' @param well_summary A data frame containing well-level classifications,
#'   typically the output of \code{classify_growth_curve_types()}.
#' @param variables A character vector specifying grouping variables that define
#'   experimental conditions.
#' @param annotation_paths A character vector of file paths to annotation files.
#'   Each file must contain at least \code{"Plate.Type"} and \code{"Well"} columns.
#' @param delimiters Optional character vector specifying delimiters for each
#'   annotation file (e.g., \code{","}, \code{"\t"}). If \code{NULL}, all files
#'   are assumed to be comma-separated.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Reads and combines multiple annotation files into a single table.
#'   \item Standardizes column names (e.g., \code{"Plate Type"} to \code{"Plate.Type"}).
#'   \item Merges annotation data with AUC estimates based on plate and well identifiers.
#'   \item Optionally classifies wells into control categories based on chemical annotations.
#'   \item Appends curve classification metadata from \code{well_summary}.
#'   \item Removes intermediate or redundant columns to produce a clean output.
#' }
#'
#' Control classification is currently based on the \code{Chemical} column, if
#' present. Wells are labeled as:
#' \itemize{
#'   \item \code{"negative"} for known negative controls (e.g., "Negative Control").
#'   \item \code{"positive"} for known positive controls (e.g., glucose conditions).
#' }
#'
#' If the \code{Chemical} column is absent, control classification is skipped.
#'
#' @return A data frame containing AUC estimates enriched with annotation data,
#'   experimental metadata, and optional control classifications.
#'
#' @import dplyr
#' @import magrittr
#' @importFrom readr read_csv
#' @importFrom readr read_delim
#' @importFrom purrr map2
#' 
#' @export
prepare_auc_annotations <- function(auc_results,
                                    well_summary,
                                    variables,
                                    annotation_paths,
                                    delimiters = NULL) {
  
  # ---- Input validation ----
  if (length(annotation_paths) == 0) {
    stop("'annotation_paths' cannot be empty.")
  }
  
  if (!all(file.exists(annotation_paths))) {
    stop("One or more annotation files do not exist.")
  }
  
  if (!is.null(delimiters) &&
      length(delimiters) != length(annotation_paths)) {
    stop("'delimiters' must match length of 'annotation_paths'.")
  }
  
  if (is.null(delimiters)) {
    delimiters <- rep(",", length(annotation_paths))
  }
  
  # ---- Step 1: Read annotation files ----
  annotations_list <- purrr::map2(annotation_paths, delimiters, ~{
    
    if (.y == ",") {
      readr::read_csv(.x, show_col_types = FALSE)
    } else {
      readr::read_delim(
        .x,
        delim = .y,
        escape_double = FALSE,
        trim_ws = TRUE,
        show_col_types = FALSE
      )
    }
    
  })
  
  # ---- Step 2: Combine and deduplicate annotations ----
  annotations <- dplyr::bind_rows(annotations_list) %>%
    dplyr::distinct()
  
  # ---- Step 3: Standardize column names ----
  if ("Plate Type" %in% names(annotations)) {
    annotations <- dplyr::rename(
      annotations,
      Plate.Type = `Plate Type`
    )
  }
  
  # ---- Step 4: Validate required annotation columns ----
  required_cols <- c("Plate.Type", "Well")
  if (!all(required_cols %in% names(annotations))) {
    stop("Annotation data must contain 'Plate.Type' and 'Well' columns.")
  }
  
  # ---- Step 5: Merge AUC results with annotations ----
  auc_ci_df <- dplyr::left_join(
    auc_results,
    annotations,
    by = c("Plate.Type", "Well")
  )
  
  # ---- Step 6: Control classification (if available) ----
  if ("Chemical" %in% names(auc_ci_df)) {
    
    auc_ci_df <- dplyr::mutate(
      auc_ci_df,
      control = dplyr::case_when(
        .data$Chemical %in% c("Negative Control", "No Chemical") ~ "negative",
        .data$Chemical %in% c("a-D-Glucose", "D-(+)-Glucose")    ~ "positive",
        TRUE ~ NA_character_
      )
    )
    
  } else {
    warning("'Chemical' column not found; skipping control classification.")
  }
  
  # ---- Step 7: Append curve classification metadata ----
  auc_ci_df <- dplyr::left_join(
    auc_ci_df,
    well_summary,
    by = c("Plate.Type", "Well", variables)
  )
  
  # ---- Step 8: Remove auxiliary columns ----
  drop_cols <- c("has_growth", "has_drop", "all_flat", "max_od")
  drop_cols <- drop_cols[drop_cols %in% names(auc_ci_df)]
  
  auc_ci_df <- dplyr::select(
    auc_ci_df,
    -dplyr::all_of(drop_cols)
  )
  
  return(auc_ci_df)
}