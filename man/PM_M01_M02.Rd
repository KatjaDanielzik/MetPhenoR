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
\examples{
data(PM_M01_M02)
head(data)
}
\keyword{datasets}
