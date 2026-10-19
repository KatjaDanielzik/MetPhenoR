#' Annotate assay data with metadata
#'
#' Integrates experimental assay data with one or more annotation tables
#' to assign chemical or treatment identities to wells.
#'
#' @param data A list containing a data frame element \code{auc_ratio},
#'   typically produced by \code{read_folder()}. The data frame must include
#'   columns \code{"Plate.Type"}, \code{"Well"}, and \code{"Chemical"}.
#' @param annotation A data frame or a list of data frames containing annotation
#'   metadata. Each must include columns \code{"Plate.Type"}, \code{"Well"},
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
#' processed_data <- compute_auc_ci(processed_data, n_boot = 10)
#' processed_data <- compute_auc_ratio(processed_data)
#' processed_data <- annotate_metadata(processed_data, annotation = PM_M01_M02)
#'
#' @export
annotate_metadata <- function(data, annotation) {
  # ---- Validate top-level structure ----
  if (!is.list(data) || is.data.frame(data) ||
    !"auc_ratio" %in% names(data)) {
    stop("'data' must be a named list containing an 'auc_ratio' element,
          as produced by compute_auc_ratio().")
  }

  # ---- Validate auc_ratio ----
  data_df <- data$auc_ratio

  if (!is.data.frame(data_df)) {
    stop("'data$auc_ratio' must be a data.frame.")
  }

  if (nrow(data_df) == 0) {
    stop("'data$auc_ratio' must contain at least one row.")
  }

  # ---- Validate required columns in data$auc_ratio ----
  required_data_cols <- c("Chemical", "Plate.Type", "Well")
  missing_data_cols <- setdiff(required_data_cols, names(data_df))
  if (length(missing_data_cols) > 0) {
    stop(paste(
      "'data$auc_ratio' is missing required columns:",
      paste(missing_data_cols, collapse = ", ")
    ))
  }

  # ---- Validate annotation type ----
  if (is.data.frame(annotation)) {
    annotations_list <- list(annotation)
  } else if (is.list(annotation) && all(purrr::map_lgl(annotation, is.data.frame))) {
    annotations_list <- annotation
  } else {
    stop("'annotation' must be a data.frame or a list of data.frames.")
  }

  # ---- Validate required columns in each annotation table individually ----
  required_ann_cols <- "Chemical"
  purrr::iwalk(annotations_list, function(df, idx) {
    missing <- setdiff(required_ann_cols, names(df))
    if (length(missing) > 0) {
      stop(paste0(
        "Annotation table ", idx,
        " is missing required columns: ",
        paste(missing, collapse = ", ")
      ))
    }
  })

  # ---- Combine and deduplicate annotations ----
  annotations <- dplyr::bind_rows(annotations_list) %>%
    dplyr::distinct()

  # ---- Warn if annotations are empty after deduplication ----
  if (nrow(annotations) == 0) {
    warning(
      "No annotation data was provided or all annotation rows were duplicates. ",
      "No metadata will be merged."
    )
  }

  # ---- Prepare data: drop Plate.Type and Well, deduplicate ----
  data_df <- data_df %>%
    dplyr::select(-.data[["Plate.Type"]], -.data[["Well"]]) %>%
    dplyr::distinct()

  # ---- Warn if Chemical column in data is all NA ----
  n_chemicals <- sum(!is.na(data_df$Chemical))
  if (n_chemicals == 0) {
    warning(
      "The 'Chemical' column in 'data$auc_ratio' contains no valid values. ",
      "No metadata can be merged."
    )
  }

  # ---- Merge ----
  data_df <- dplyr::left_join(
    data_df,
    annotations,
    by = required_ann_cols
  )

  # ---- Warn if chemicals not found in annotation ----
  data_chemicals <- unique(data_df$Chemical[!is.na(data_df$Chemical)])
  annot_chemicals <- unique(annotations$Chemical[!is.na(annotations$Chemical)])
  missing_chemicals <- setdiff(data_chemicals, annot_chemicals)

  if (length(missing_chemicals) > 0) {
    if (length(missing_chemicals) <= 10) {
      warning(
        "The following chemicals were not found in the annotation table: ",
        paste(missing_chemicals, collapse = ", "),
        ". No metadata will be assigned to wells with these chemicals."
      )
    } else {
      warning(
        "The following ", length(missing_chemicals),
        " chemicals were not found in the annotation table: ",
        paste(head(missing_chemicals, 5), collapse = ", "),
        "... (and ", length(missing_chemicals) - 5, " more).",
        " No metadata will be assigned to wells with these chemicals."
      )
    }
  }

  # ---- Return ----
  data$auc_ratio <- data_df
  return(data)
}
