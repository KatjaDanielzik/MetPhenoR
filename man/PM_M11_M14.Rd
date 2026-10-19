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
    \item{\samp{Plate.Type}}{Odin Plate Type}
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
}
\examples{
data("PM_M11_M14")
head(PM_M11_M14)
}
\keyword{datasets}
