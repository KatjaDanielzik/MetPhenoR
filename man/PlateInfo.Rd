\name{PlateInfo}
\alias{PlateInfo}
\docType{data}
\title{
A Capitalized Title for the Data Set
}
\usage{data("PlateInfo")}
\format{
  A data frame with 1 observations on the following 13 variables.
  \describe{
    \item{\code{PlateId}}{a character vector}
    \item{\samp{Project Name}}{a character vector}
    \item{\samp{Project Code}}{a character vector}
    \item{\samp{Plate Type}}{a character vector}
    \item{\code{Sample}}{a character vector}
    \item{\samp{Loaded At}}{a POSIXct}
    \item{\samp{Incubation Hours}}{a numeric vector}
    \item{\samp{Incubation Temperature Celsius}}{a numeric vector}
    \item{\samp{Incubated Offline}}{a logical vector}
    \item{\samp{Cycle Time Minutes}}{a numeric vector}
    \item{\code{Position}}{a character vector}
    \item{\code{Protocol}}{a logical vector}
    \item{\code{Description}}{a logical vector}
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

PlateInfo <- PlateInfo[PlateInfo$PlateId==RawRead$PlateId[1],]
PlateInfo$Sample <- gsub(" ","_",PlateInfo$Sample)
}
\examples{
data(PlateInfo)
head(PlateInfo)
}
\keyword{datasets}
