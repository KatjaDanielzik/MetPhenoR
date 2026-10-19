#' Read "PlaateInfo" and "RawReads" file robustly for "," and ";" separated
#'
#' @param x file name
#' @returns returns a read in file
#' @keywords internal
#' @importFrom readr read_delim
.read_with_fallback_delimiter_with_warning <- function(x, ...) {
  # Try reading with comma delimiter
  data_comma <- tryCatch(
    {
      readr::read_delim(x, delim = ",", col_names = TRUE, ...)
    },
    error = function(e) NULL
  )

  # Check if comma reading succeeded and column exists
  if (!is.null(data_comma) && "PlateId" %in% names(data_comma)) {
    file <- data_comma
  }

  # Try semicolon delimiter
  data_semicolon <- tryCatch(
    {
      readr::read_delim(x, delim = ";", col_names = TRUE, ...)
    },
    error = function(e) NULL
  )

  # Check if semicolon reading succeeded and column exists
  if (!is.null(data_semicolon) && "PlateId" %in% names(data_semicolon)) {
    file <- data_semicolon
  }

  # If both fail, issue a warning
  if (!is.data.frame(file)) {
    warning(paste0(
      "File '", x, "' was not successfully read. ",
      "Please ensure the file is either comma- or semicolon-separated."
    ))
  }

  # Replace spaces with dots for consistent naming of columns (e.g. Plate Types)
  names(file) <- gsub(" ", ".", names(file))

  return(file)
}


#' Align timestamps 'Read At' between 590 nm and 740 nm
#'
#' Takes timestamps from Wavelength == 590 and writes them to the corresponding 740nm entries
#' assuming that time stamps deviate by little amount (e.g. 15s)
#' @param x a raw Odin read
#'
#' @returns same time points for 590 and 740 Wavelength assuming that there is
#' only a negligible difference in time recordings between these two time points
#' @keywords internal
#' @importFrom dplyr select
#' @importFrom dplyr filter
.adjust_timestamp <- function(x) {
  Wavelength <- NULL
  Read.At <- NULL
  # get time stamps for 590 nm
  timestamps <- x %>%
    dplyr::filter(Wavelength == "590") %>%
    dplyr::select(Read.At)

  temp <- x %>%
    dplyr::filter(Wavelength == "740") # assume same read time for 740 and 590
  temp$Read.At <- timestamps$Read.At

  temp2 <- x %>%
    dplyr::filter(Wavelength == "590")
  x <- rbind(temp, temp2)
  return(x)
}


#' Calculates difference between timestamp and starttime of experiment
#'
#' @param x a raw Odin read with adjusted time stamps (see adjust_timestamp())
#'
#' @returns a Odin read with a new column 'Hour' indicating the difference between
#' 'Read At' and first element of 'Read At'
#' @keywords internal
#' @importFrom dplyr mutate
#' @importFrom dplyr relocate

.calculate_diff_starttime <- function(x) {
  Read.At <- NULL
  starttime <- NULL
  Hour <- NULL
  x$starttime <- x$Read.At[1]
  x <- x %>%
    dplyr::mutate(Hour = as.numeric(difftime(Read.At, starttime, units = "hours"))) %>%
    dplyr::relocate(Hour, .after = Read.At) %>%
    dplyr::select(-starttime)
  return(x)
}

#' Noise correction of optical density measurements (590nm-740nm)
#'
#' @param x a processed Odin read (see functions .align_timestamps, .caluclate_diff_starttime)
#' @returns an Odin read with a column added diff_OD_590_740
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr left_join
#' @importFrom dplyr distinct
#' @keywords internal
.correct_OD <- function(x) {
  Wavelength <- NULL
  A01 <- NULL
  H12 <- NULL
  PlateId <- NULL
  Hour <- NULL
  Well <- NULL
  OD <- NULL
  `590` <- NULL
  `740` <- NULL
  Id <- NULL

  # store all ID variables
  temp <- x %>%
    dplyr::select(-c(Wavelength, A01:H12, Id)) %>%
    dplyr::distinct()
  # caluclate diff_OD_590_740
  x <- x %>%
    tidyr::pivot_longer(
      cols = A01:H12,
      names_to = "Well",
      values_to = "OD"
    ) %>%
    dplyr::select(PlateId, Hour, Wavelength, Well, OD) %>%
    tidyr::pivot_wider(
      names_from = Wavelength,
      values_from = OD
    ) %>%
    dplyr::mutate(diff_OD_590_740 = `590` - `740`) %>%
    dplyr::select(-c(`740`, `590`)) %>%
    dplyr::distinct()
  # join all ID variables
  x <- dplyr::left_join(temp, x)

  return(x)
}
