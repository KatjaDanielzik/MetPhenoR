\name{RawRead}
\alias{RawRead}
\docType{data}
\title{
Example Raw Read from Odin Biolog
}
\usage{data("RawRead")}
\format{
  A data frame with 288 observations on the following 102 variables.
  \describe{
    \item{\code{Id}}{a character vector}
    \item{\code{PlateId}}{a character vector}
    \item{\code{Wavelength}}{a numeric vector}
    \item{\samp{Actual Temperature Celsius}}{a numeric vector}
    \item{\samp{Target Temperature Celsius}}{a numeric vector}
    \item{\samp{Read At}}{a POSIXct}
    \item{\code{A01}}{a numeric vector}
    \item{\code{A02}}{a numeric vector}
    \item{\code{A03}}{a numeric vector}
    \item{\code{A04}}{a numeric vector}
    \item{\code{A05}}{a numeric vector}
    \item{\code{A06}}{a numeric vector}
    \item{\code{A07}}{a numeric vector}
    \item{\code{A08}}{a numeric vector}
    \item{\code{A09}}{a numeric vector}
    \item{\code{A10}}{a numeric vector}
    \item{\code{A11}}{a numeric vector}
    \item{\code{A12}}{a numeric vector}
    \item{\code{B01}}{a numeric vector}
    \item{\code{B02}}{a numeric vector}
    \item{\code{B03}}{a numeric vector}
    \item{\code{B04}}{a numeric vector}
    \item{\code{B05}}{a numeric vector}
    \item{\code{B06}}{a numeric vector}
    \item{\code{B07}}{a numeric vector}
    \item{\code{B08}}{a numeric vector}
    \item{\code{B09}}{a numeric vector}
    \item{\code{B10}}{a numeric vector}
    \item{\code{B11}}{a numeric vector}
    \item{\code{B12}}{a numeric vector}
    \item{\code{C01}}{a numeric vector}
    \item{\code{C02}}{a numeric vector}
    \item{\code{C03}}{a numeric vector}
    \item{\code{C04}}{a numeric vector}
    \item{\code{C05}}{a numeric vector}
    \item{\code{C06}}{a numeric vector}
    \item{\code{C07}}{a numeric vector}
    \item{\code{C08}}{a numeric vector}
    \item{\code{C09}}{a numeric vector}
    \item{\code{C10}}{a numeric vector}
    \item{\code{C11}}{a numeric vector}
    \item{\code{C12}}{a numeric vector}
    \item{\code{D01}}{a numeric vector}
    \item{\code{D02}}{a numeric vector}
    \item{\code{D03}}{a numeric vector}
    \item{\code{D04}}{a numeric vector}
    \item{\code{D05}}{a numeric vector}
    \item{\code{D06}}{a numeric vector}
    \item{\code{D07}}{a numeric vector}
    \item{\code{D08}}{a numeric vector}
    \item{\code{D09}}{a numeric vector}
    \item{\code{D10}}{a numeric vector}
    \item{\code{D11}}{a numeric vector}
    \item{\code{D12}}{a numeric vector}
    \item{\code{E01}}{a numeric vector}
    \item{\code{E02}}{a numeric vector}
    \item{\code{E03}}{a numeric vector}
    \item{\code{E04}}{a numeric vector}
    \item{\code{E05}}{a numeric vector}
    \item{\code{E06}}{a numeric vector}
    \item{\code{E07}}{a numeric vector}
    \item{\code{E08}}{a numeric vector}
    \item{\code{E09}}{a numeric vector}
    \item{\code{E10}}{a numeric vector}
    \item{\code{E11}}{a numeric vector}
    \item{\code{E12}}{a numeric vector}
    \item{\code{F01}}{a numeric vector}
    \item{\code{F02}}{a numeric vector}
    \item{\code{F03}}{a numeric vector}
    \item{\code{F04}}{a numeric vector}
    \item{\code{F05}}{a numeric vector}
    \item{\code{F06}}{a numeric vector}
    \item{\code{F07}}{a numeric vector}
    \item{\code{F08}}{a numeric vector}
    \item{\code{F09}}{a numeric vector}
    \item{\code{F10}}{a numeric vector}
    \item{\code{F11}}{a numeric vector}
    \item{\code{F12}}{a numeric vector}
    \item{\code{G01}}{a numeric vector}
    \item{\code{G02}}{a numeric vector}
    \item{\code{G03}}{a numeric vector}
    \item{\code{G04}}{a numeric vector}
    \item{\code{G05}}{a numeric vector}
    \item{\code{G06}}{a numeric vector}
    \item{\code{G07}}{a numeric vector}
    \item{\code{G08}}{a numeric vector}
    \item{\code{G09}}{a numeric vector}
    \item{\code{G10}}{a numeric vector}
    \item{\code{G11}}{a numeric vector}
    \item{\code{G12}}{a numeric vector}
    \item{\code{H01}}{a numeric vector}
    \item{\code{H02}}{a numeric vector}
    \item{\code{H03}}{a numeric vector}
    \item{\code{H04}}{a numeric vector}
    \item{\code{H05}}{a numeric vector}
    \item{\code{H06}}{a numeric vector}
    \item{\code{H07}}{a numeric vector}
    \item{\code{H08}}{a numeric vector}
    \item{\code{H09}}{a numeric vector}
    \item{\code{H10}}{a numeric vector}
    \item{\code{H11}}{a numeric vector}
    \item{\code{H12}}{a numeric vector}
  }
}
\source{
  # data read in 
  ## list all files including all subdirectories
  data_path <- "~/Documents/Metabolomics/odin_pipeline/data/ODIN_Sarah_Etemadi_Afshar/CSV for Katja/Drug plates/H460/"
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
  
  RawRead <- RawReads[[1]]
  use_data(RawRead)
}
\examples{
data(RawRead)
head(RawRead)
}
\keyword{datasets}
