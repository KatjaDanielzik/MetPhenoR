\name{PM_M01_M02}
\alias{PM_M01_M02}
\docType{data}
\title{
Annotations for metabolic plates PM-M01 and PM-M02
}
\description{
Mapping metabolic plate wells to corresponding Chemical and KEGG annotations
}
\usage{data("PM_M01_M02")}
\format{
  A data frame with 201 observations on the following 11 variables.
  \describe{
    \item{\samp{Plate Type}}{Odin Plate Type}
    \item{\code{Well}}{Well ID on plate}
    \item{\code{Chemical}}{Chemical plated in 'Well' in 'PlateType'}
    \item{\samp{Kegg Link}}{Link to KEGG database}
    \item{\code{Info}}{e.g. 'Carbon Source'}
    \item{\code{KEGG}}{KEGG ID of 'Chemical'}
    \item{\code{module_id}}{KEGG module ID, can be multiple per 'KEGG'}
    \item{\code{module_name}}{KEGG module name}
    \item{\code{upper_hierarchy}}{upper hierarchy annotation of 'module_id'}
    \item{\code{middle_hierarchy}}{middle hierarchy annotation of 'module_id}
    \item{\code{lower_hierarchy}}{lower hierarchy annotation of 'lower_hierarchy}
  }
}
\details{
source: PM-M[01] Annotation Exported (from Odin Biolog: PlateId + Well + Chemical), data/annotation/KEGG_background.csv
script: scripts/KEGGIDs_from_exported_Odin_annotation
some KEGG IDs (comment) were wrong and one could be added by hand
includes pathway and functional module annotation from KEGG (potentially multiple pathway annotations!)

Katja Danielzik hand curated the a-D-Glucose KEGG ID to C00267 (as also Julia  Asche annotated it) inside the script
}
\source{
# file data/annotations/PM-M01_Annotation_Exported contains Kegg Links
# that contain the KEGG IDs after "?". I want to split the string to obtain the 
# KEGG IDs

# Read in files
library(readr)
library(tidyverse)
PM_M01 <-  read_delim("data/annotations/PM-M01 Annotation Exported.csv", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)

PM_M02 <-  read_delim("data/annotations/PM-M02 Annotation Exported.csv", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)

PM_M01_M02 <- rbind(PM_M01,PM_M02)

# recover KEGG ID from link: KEGG ID is everything behind "?"
x <- do.call(rbind, strsplit(x = PM_M01_M02$`Kegg Link`, split = "bget")) # "?" does not work with gsub
x <- substr(x[,3], start = 2,stop = max(na.omit(nchar(x[,3])))) # remove ? at first position of character strings
PM_M01_M02$KEGG <- x

# swap KEGG ID of a-D-Glucose from C00031 to C00267
PM_M01_M02 <- PM_M01_M02%>%mutate(KEGG=ifelse(Chemical=="a-D-Glucose","C00267",Chemical),
                                  `Kegg Link`=ifelse(Chemical=="a-D-Glucose","https://www.genome.jp/dbget-bin/www_bget?cpd:C00267",`Kegg Link`))

# add pathway annotation from KEGG pathway
KEGG_background <- read_csv("data/annotations/KEGG_background.csv")
KEGG_background <- KEGG_background[,-1]
PM_M01_M02 <- left_join(PM_M01_M02,KEGG_background,by="KEGG")

}
\references{
}
\examples{
data(PM_M01_M02)
head(data)
}
\keyword{datasets}
