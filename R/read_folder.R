#' Process CSV Plate Data stored in folder into data frame in long format
#' 
#' Reads all 'PlateInfo' and 'RawRead' files from a directory (can have subdirectories),
#' merges the 'PlateInfo'
#' files and annotates the RawRead files with the 'Description' column of 'PlateInfo'.
#' Proceeds by aligning the time points of OD values measured at 590 nm and 740 nm,
#' calculates '590 nm' - '740 nm' for noise correction as advised by Biolog.
#'
#' @param data_path Character. Path to directory containing CSV files. At least 
#' one file, which name ends on "_PlateInfo.csv" containing the metadata with at
#' least a column 'PlateId' and a column 'Description'
#' and at least one file containing the raw Odin reads, which name end on 
#' "_RawReads.csv" and have at least one column 'PlateId' and columns 'A01' to
#' 'H12' representing the wells of a 96-well plate.
#' @param sample_column Character. Which column in '_PlateInfo.csv' contains the
#' 'Description' (i.e. the Character vector that should be split according to 'variables')
#' @param variables Character vector. Names of variables to extract from
#'   the `Description` column (e.g., c("cell.line", "radiation.dose", "inhibitor")).
#'   Have to be separated by "_" and always in the same order for all "_PlateInfo.csv"
#'   files (e.g. in format cell.line, radiation.dose, time.point -> "A549_0Gy_0h")
#' @return 
#' #' \itemize{
#'   \item \code{result}: A data frame in long format
#'     \itemize{
#'     \item \code{}: 
#'     \item \code{}: 
#'     \item \code{}
#'     }

#' @importFrom stringr str_detect
#' @importFrom purrr list_rbind
#' @importFrom tidyr separate
#' @export
#' @examples
#' data("RawRead")
#' head(RawRead)
#' data("PlateInfo")
#' head(PlateInfo)
#' data("data")
#' head(data)
read_folder <- function(data_path, sample_column = "Description", variables) {
  
   # are all elements in variables character values?
   # is the number of _ separated columns from Description equal to number of 
   # elements in variables?
   # is data_path a valid path?
   
  # data read in 
  ## list all files including all subdirectories
  files <- list.files(path=data_path,
                      include.dirs = TRUE,
                      recursive = TRUE,
                      full.names = TRUE)
  # detect metadata in PlateInfo files
  PlateInfo <-   files[stringr::str_detect(string = files, pattern = "_PlateInfo.csv")]
  # read in Plate Info files
  PlateInfo <- lapply(PlateInfo,readr::read_csv)
  PlateInfo <- purrr::list_rbind(PlateInfo)
  # detect raw 
  RawReads <- files[stringr::str_detect(string = files, pattern ="_RawReads.csv")]
  RawReads <- lapply(RawReads,readr::read_csv)

  # align time stamps, calculate delta starttime and noise correct OD
  RawReads <- lapply(RawReads,.adjust_timestamp)
  RawReads <- lapply(RawReads,.calculate_diff_starttime)
  RawReads <- lapply(RawReads,.correct_OD)
  
  RawReads <- purrr::list_rbind(RawReads)
  
  # join RawReads and metadata: only keep entries that are in PlateInfo and RawReads
  data <- dplyr::inner_join(PlateInfo,RawReads)
  
  # !!! test if all PlateId in data are found in PlateInfo and RawReads
  # warning message PlateId not found in RawReads or PlateInfo, proceeding without
  
  # Separate metadata into biological conditions
  data <- tidyr::separate(
    data,
    sample_column,
    into = variables,
    sep = "_",
    remove = FALSE
  )
  
  return(list(data=data,variables=variables,sample_column=sample_column))
}