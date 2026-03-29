\name{PM_M11_M14}
\alias{PM_M11_M14}
\docType{data}
\title{
Annotations for anti-cancer drug plates PM-M11 to PM-M14
}
\description{
mapping of anti-cancer-drug plate wells to corresponding Chemical, KEGG ID, 
Inhibition Target, ...
}
\usage{data("PM_M11_M14")}
\format{
  A data frame with 384 observations on the following 11 variables.
  \describe{
    \item{\samp{Plate Type}}{Odin Plate Type}
    \item{\code{Well}}{Well ID on plate}
    \item{\code{Chemical}}{Chemical plated in 'Well' in 'PlateType'}
    \item{\code{Info}}{e.g. "antineoplastic, antiinflammatory"}
    \item{\code{KEGG}}{KEGG ID of 'Chemical'}
    \item{\code{CAS}}{CAS ID of 'Chemical'}
    \item{\samp{Inhibition Target}}{Inhibition Target of 'Chemical'}
    \item{\samp{Mechanism of Action}}{Mechanism of Action of 'Chemical'}
    \item{\samp{Affected Pathway}}{Affected Pathway of 'Chemical'}
    \item{\samp{Used to Treat}}{'Chemical' is used to treat ...}
    \item{\samp{Clinical Status (March 2025)}}{Clinical Status of 'Chemical' in March 2025}
  }
}
\details{
source: data/annotations PMM11.csv - PMM14.csv provided by Odin Biolog, 
data/annotations 20250312_SEA_ODIN_PMM11-14 Pathways.xlsx provided by Johann Matschke
\source{
source: data/annotations PMM11.csv - PMM14.csv, data/annotations 20250312_SEA_ODIN_PMM11-14 Pathways.xlsx
script: scripts/anti_cancer_annotation.csv
annotations of anti cancer drug plates including inhibition target, affected
pathway, used to treat and clinical status (dependent on 20250312_SEA_ODIN_PMM11-14 Pathways.xlsx)}

# create summary of anti-cancer annotations with consistent column naming ####
# and merge with metabolic plate annotation
library(readr)
library(tidyverse)
library(readxl)
# import plate annotations, without column names ####
PMM11 <- read_csv("data/annotations/PMM11.csv", 
                  col_names = FALSE)
PMM12 <- read_csv("data/annotations/PMM12.csv", 
                  col_names = FALSE)
PMM13 <- read_csv("data/annotations/PMM13.csv", 
                  col_names = FALSE)
# filter out empty rows (excel saving error)
PMM13 <- PMM13%>%filter_all(any_vars(!is.na(.)))

PMM14 <- read_csv("data/annotations/PMM14.csv", 
                  col_names = FALSE)
PM11_14 <- rbind(PMM11,PMM12,PMM13,PMM14)
rm(PMM11,PMM12,PMM13,PMM14)

# remove empty columns and assign column names according to PM_M01_M02 annotation

PM11_14 <- PM11_14 %>% select_if(~!(all(is.na(.)) | all(. == "")))
colnames(PM11_14) <- c("Plate Type","Well","Chemical","Info","KEGG","CAS")

# clean up CAS IDs
PM11_14$CAS <- gsub("CAS ","",PM11_14$CAS)

## Elaidyphosphocholine does not have CAS ID
PM11_14$CAS <- gsub("Elaidylphosphocholine","No CAS ID",PM11_14$CAS)

# add drug annotations
SEA_ODIN_PMM11_14_Pathways <- read_excel("data/annotations/20250312_SEA_ ODIN_PMM11-14 Pathways.xlsx")

PM11_14 <- left_join(PM11_14,SEA_ODIN_PMM11_14_Pathways,join_by(Chemical==`Chemical Compound`))
rm(SEA_ODIN_PMM11_14_Pathways)

# change plate type names according to naming from Odin
PM11_14$`Plate Type` <- gsub("PMM11","PM-M11",PM11_14$`Plate Type`)
PM11_14$`Plate Type` <- gsub("PMM12","PM-M12",PM11_14$`Plate Type`)
PM11_14$`Plate Type` <- gsub("PMM13","PM-M13",PM11_14$`Plate Type`)
PM11_14$`Plate Type` <- gsub("PMM14","PM-M14",PM11_14$`Plate Type`)


write.csv(PM11_14,"data/annotations/PM11_14.csv",row.names = FALSE)
\examples{
data(PM_M11_M14)
head(PM_M11_M14)
}
\keyword{datasets}
