#' Process CSV Plate Data stored in folder into data frame in long format
#'
#' Reads multiple CSV files from a directory, merges them, reshapes
#' well-based measurements into long format, and extracts metadata
#' variables from a description column.
#'
#' @param data_path Character. Path to directory containing CSV files.
#' @param variables Character vector. Names of variables to extract from
#'   the `Description` column (e.g., c("cell_line", "radiation.dose", "inhibitor")).
#' @importFrom dplyr rbind_rows
#' @return A tidy data frame in long format.
#' @export
read_folder <- function(data_path, variables) {
  
  # ---- Input validation ----
  if (!dir.exists(data_path)) {
    stop("The provided data_path does not exist.")
  }
  
  if (missing(variables) || length(variables) == 0) {
    stop("You must provide 'variables' as a non-empty character vector.")
  }
  
  # ---- Read CSV files ----
  raw_files <- list.files(data_path, pattern = "\\.csv$", full.names = TRUE)
  
  if (length(raw_files) == 0) {
    stop("No CSV files found in the provided directory.")
  }
  
  list_of_dfs <- lapply(raw_files, utils::read.csv)
  
  combined_raw <- dplyr::bind_rows(list_of_dfs)
  
  # ---- Identify well columns ----
  well_cols <- grep("^[A-H][0-9]{2}$", names(combined_raw), value = TRUE)
  
  if (length(well_cols) == 0) {
    stop("No well columns (A01-H12) detected in the dataset.")
  }
  
  # !!! keep the
  # ---- Pivot to long format ----
  combined_long <- tidyr::pivot_longer(
    combined_raw,
    cols = tidyselect::all_of(well_cols),
    names_to = "Well",
    values_to = "diff_OD_590_740"
  )
  
  # ---- Safe column removal ----
  cols_to_remove <- c("Data.File", "Setup.Time", "Position", "cell.line")
  
  combined_long <- dplyr::select(
    combined_long,
    -dplyr::any_of(cols_to_remove)
  )
  
  combined_long <- dplyr::distinct(combined_long)
  
  # ---- Validate Description column ----
  if (!"Description" %in% names(combined_long)) {
    stop("Column 'Description' not found in the dataset.")
  }
  
  # ---- Separate metadata ----
  combined_long <- tidyr::separate(
    combined_long,
    Description,
    into = variables,
    sep = "_",
    remove = FALSE
  )
  
  return(combined_long)
}