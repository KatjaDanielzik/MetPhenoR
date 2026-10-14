#' Process CSV Plate Data stored in folder into data frame in long format
#' 
#' Reads all 'PlateInfo' and 'RawRead' files from a directory (can have subdirectories),
#' merges the 'PlateInfo'
#' files and annotates the RawRead files with the 'Description' column of 'PlateInfo'.
#' Proceeds by aligning the time points of OD values measured at 590 nm and 740 nm,
#' calculates '590 nm' - '740 nm' for noise correction as advised by Biolog.
#'
#' @param data_path Character. Path to directory containing CSV files. At least 
#' one file, which name ends on "PlateInfo.csv" containing the metadata with at
#' least a column 'PlateId' and the 'sample_column' containing the description of the experiment.
#' and at least one file containing the raw Odin reads, which name end on 
#' "RawReads.csv" and have at least one column 'PlateId' and columns 'A01' to
#' 'H12' representing the wells of a 96-well plate.
#' @param sample_column Character. Which column in '_PlateInfo.csv' contains the
#' 'Description' (i.e. the Character vector that should be split according to 'variables')
#' @param variables Character vector. Names of variables to extract from
#'   the 
#'   'Description' column (e.g., c("cell.line", "radiation.dose", "inhibitor")).
#'   Have to be separated by "_" and always in the same order for all "_PlateInfo.csv"
#'   files (e.g. in format cell.line, radiation.dose, time.point -> "A549_0Gy_0h")
#'
#' @return 
#' \describe{ a list
#'  \itemize{
#'   \item \code{result}: A data frame in long format
#'     \itemize{
#'     \item \code{PlateId}: Odin plate Id from 'PlateInfo', refers to a single 96-well plate
#'     \item \code{Project Name}: Odin project name from 'PlateInfo' 
#'     \item \code{Project Code}: Odin project code from 'PlateInfo'
#'     \item \code{Plate Type}: Odin plate type from 'PlateInfo'
#'     \item \code{Sample}: Sample specification from 'PlateInfo'
#'     \item \code{Loaded At}: Loading time from 'PlateInfo'
#'     \item \code{Incubation Hours}: Incubation time from 'PlateInfo'
#'     \item \code{Incubation Temperature Celsius}: Incubation temperature from 'PlateInfo'
#'     \item \code{Incubated Offline}: 
#'     \item \code{Cycle Time Minutes}: Time interval of measurements from 'PlateInfo'
#'     \item \code{Position}: Position of Plate in Odin from 'PlateInfo'
#'     \item \code{Protocol}: user specified protocol from 'PlateInfo'
#'     \item \code{Description}: user specified description of the experiment (or in 'Sample')
#'     \item \code{...}: single variable columns from 'sample_column'
#'     \item \code{Actual Temperature Celsius}: measured temperature in Odin from 'RawRead'
#'     \item \code{Target Temperature Celsius}: Target temperature (should be equal to Incubation Temperature Celsius from 'PlateInfo')
#'     \item \code{Read At}: Time of Plate read from 'RawReads'
#'     \item \code{Hour}: difference between Read At and first entry in Read At per plate 
#'     \item \code{Well}: Well Id of the 96-well plate
#'     \item \code{diff_OD_590_740}: OD_590nm-OD_740nm, noise correction for mammalian plates according to Biolog  
#'     }
#'  \itemize{
#'   \item \code{meta}: A list
#'     \itemize{
#'     \item \code{data_path}: user set 'data_path'
#'     \item \code{sample_column}: user set 'sample_column' 
#'     \item \code{variables}: user set 'variables'
#'     }
#'  }
#' }
#' }
#' @importFrom readr read_csv
#' @importFrom stringr str_detect
#' @importFrom purrr list_rbind
#' @importFrom tidyr separate
#'
#' @export
#' @examples
#' # Raw Reads are in this format
#' data("RawRead")
#' head(RawRead)
#' 
#' # PlateInfo is in this format
#' data("PlateInfo")
#' head(PlateInfo)
#' 
#' # resulting data frame 'data' from read_folder
#' data("processed_data")
#' head(processed_data$data)
read_folder <- function(data_path, sample_column, variables) {
  
  if (!dir.exists(data_path)) {
    stop("data_path does not exist or is not a valid directory")
  }
  if (length(variables) == 0) {
    stop("'variables' must contain at least one element")
  }
  
  if (any(!is.character(variables))) {
    stop("All elements in variables must be character strings")
  }
  
  if(!is.character(sample_column)){
    stop("'sample_column' must be a character string")
  }
  
  PlateId <- NULL
  
  # data read in 
  ## list all files including all subdirectories
  files <- list.files(path=data_path,
                      include.dirs = TRUE,
                      recursive = TRUE,
                      full.names = TRUE)
  # detect metadata in PlateInfo files
  PlateInfo <-   files[stringr::str_detect(string = files, pattern = "PlateInfo.csv")]
  # read in Plate Info files
  PlateInfo <- lapply(PlateInfo,.read_with_fallback_delimiter_with_warning,
                      trim_ws = TRUE)
  
  # check if any PlateInfo column is double
  PlateInfo <- lapply(PlateInfo, function(df) {
    # Convert all numeric columns (including double) to character
      if (is.double(df$Sample)) {
        df$Sample <- as.character(df$Sample)
      }
    return(df)
  })
  
  PlateInfo <- purrr::list_rbind(PlateInfo)
  
  if (!sample_column %in% names(PlateInfo)) {
    stop("'sample_column' must be a column in 'PlateInfo.csv' files")
  }
  
  # detect raw 
  RawReads <- files[stringr::str_detect(string = files, pattern ="RawReads.csv")]
  RawReads <- lapply(RawReads,.read_with_fallback_delimiter_with_warning)

  # align time stamps, calculate delta starttime and noise correct OD
  RawReads <- lapply(RawReads,.adjust_timestamp)
  RawReads <- lapply(RawReads,.calculate_diff_starttime)
  RawReads <- lapply(RawReads,.correct_OD)
  
  RawReads <- purrr::list_rbind(RawReads)
  
  # join RawReads and metadata: only keep entries that are in PlateInfo and RawReads
  data <- dplyr::inner_join(PlateInfo,RawReads)
  
  # Separate metadata into biological condition
  # Join RawReads and metadata: only keep entries that are in both PlateInfo and RawReads (inner_join)
  # Provide feedback on which PlateIds were used vs. excluded
  all_plate_ids <- unique(c(PlateInfo$PlateId, RawReads$PlateId))
  used_plate_ids <- intersect(PlateInfo$PlateId, RawReads$PlateId)
  
  # Identify excluded PlateIds
  excluded_plate_ids <- setdiff(all_plate_ids, used_plate_ids)
  
  # Create a summary of used and excluded plates
  if (length(excluded_plate_ids) > 0) {
    # Get description/sample for excluded plates
    excluded_info <- PlateInfo %>%
      dplyr::filter(PlateId %in% excluded_plate_ids) %>%
      dplyr::select(PlateId, !!sym(sample_column)) %>%
      dplyr::arrange(PlateId)
    
    warning(
      "The following 'sample_column' were found in either 'PlateInfo.csv' or 'RawReads.csv' files but not in both, and were excluded:\n",
      paste0("  - ", paste(paste0(unique(excluded_info[[sample_column]], ")")), collapse = "\n  - ")),
      "\nProceeding with only the overlapping PlateIds."
    )
  } else {
    message("All PlateIds were successfully matched between PlateInfo and RawReads.")
  }
  
  
  data <- tidyr::separate(
    data,
    sample_column,
    into = variables,
    sep = "_",
    remove = FALSE
  )
  
  return(list(data=data,meta=list(data_path=data_path,sample_column=sample_column,variables=variables)))
}