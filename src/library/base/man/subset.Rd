\name{subset}
\alias{subset}
\alias{subset.default}
\alias{subset.data.frame}
\title{Subsetting Vectors and Data Frames}
\description{
  Return subsets of vectors or data frames which meet conditions.
}
\usage{
subset(x, \dots)
subset.default(x, subset, \dots)
subset.data.frame(x, subset, select, \dots)
}
\arguments{
  \item{x}{object to be subsetted}
  \item{\dots}{how to subset, depends on object}
  \item{subset}{logical expression}
  \item{select}{expression, indicating variables to select from a
    data frame}
}
\details{
  For ordinary vectors, the result is simply
  \code{x[subset & !is.na(subset)]}.

  For dataframes, the \code{subset} argument works similarly on the
  rows.  Note that \code{subset} will be evaluated in the dataframe.

  The \code{select} argument exists only for dataframes.  It works by
  first replacing variable names in the selection expression with the
  corresponding column numbers in the dataframe and then using the
  resulting integer vector to index the columns.  This allows the use
  of the standard indexing conventions so that for examples ranges of
  variables can be specified easily.
}
\value{
  Selected rows and columns of the object \code{x}.
}
\author{Peter Dalgaard}
\seealso{
  \code{\link{[}}, % = ./Extract.Rd
  \code{\link{transform}}
}
\examples{
data(airquality)
subset(airquality, Temp > 80, select = c(Ozone, Temp))
subset(airquality, Day == 1, select = -Temp)
subset(airquality, select = Ozone:Wind)

attach(airquality)
subset(Ozone, Temp > 80)
}
\keyword{manip}
