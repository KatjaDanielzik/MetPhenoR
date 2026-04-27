\name{processed_data}
\alias{processed_data}
\docType{data}
\title{
Example data set as result of read_folder.R with sample_column = "Sample" and
variables = c("cell.line", "LKB1.status", "radiation.dose")
}
\description{
results of read_folder() function
processed_data <- read_folder("your data path",variables = c("cell.line","radiation.dose","inhibitior","time.h"))
}
\usage{data("processed_data")}
\format{
\describe{ a list
  \itemize{
   \item \code{data}: a data frame
    \itemize{
    \item{\code{PlateId}: a character vector}
    \item{\samp{Project Name}: a character vector}
    \item{\samp{Project Code}: a character vector}
    \item{\samp{Plate Type}: a character vector}
    \item{\code{Sample}: a character vector}
    \item{\code{cell.line}: a character vector}
    \item{\code{LKB1.status}: a character vector}
    \item{\code{radiation.dose}: a character vector}
    \item{\samp{Loaded At}: a POSIXct}
    \item{\samp{Incubation Hours}: a numeric vector}
    \item{\samp{Incubation Temperature Celsius}: a numeric vector}
    \item{\samp{Incubated Offline}: a logical vector}
    \item{\samp{Cycle Time Minutes}: a numeric vector}
    \item{\code{Position}: a character vector}
    \item{\code{Protocol}: a logical vector}
    \item{\code{Description}: a logical vector}
    \item{\code{Id}: a character vector}
    \item{\samp{Actual Temperature Celsius}: a numeric vector}
    \item{\samp{Target Temperature Celsius}: a numeric vector}
    \item{\samp{Read At}: a POSIXct}
    \item{\code{Hour}: a numeric vector}
    \item{\code{Well}: a character vector}
    \item{\code{diff_OD_590_740}: a numeric vector}
    }
  }
  \itemize{
   \item \code{meta}: A list
     \itemize{
     \item \code{data_path}: user set 'data_path'
     \item \code{sample_column}: user set 'sample_column' 
     \item \code{variables}: user set 'variables'
     }
    }
  }
}
\examples{
data("processed_data")
head(processed_data)
}
\keyword{datasets}
