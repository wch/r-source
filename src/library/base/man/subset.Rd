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

\method{subset}{default}(x, subset, \dots)

\method{subset}{data.frame}(x, subset, select, drop = FALSE, \dots)
}
\arguments{
  \item{x}{object to be subsetted.}
  \item{subset}{logical expression.}
  \item{select}{expression, indicating columns to select from a
    data frame.}
  \item{drop}{passed on to \code{[} indexing operator.}
  \item{\dots}{further arguments to be passed to or from other methods.}
}
\details{
  For ordinary vectors, the result is simply
  \code{x[subset & !is.na(subset)]}.

  For data frames, the \code{subset} argument works similarly on the
  rows.  Note that \code{subset} will be evaluated in the data frame, so
  columns can be referred to (by name) as variables.

  The \code{select} argument exists only for the method for data frames.
  It works by
  first replacing names in the selection expression with the
  corresponding column numbers in the data frame and then using the
  resulting integer vector to index the columns.  This allows the use
  of the standard indexing conventions so that for example ranges of
  columns can be specified easily.

  The \code{drop} argument is passed on to the indexing method for data
  frames.
}
\value{
  An object similar to \code{x} contain just the selected elements (for
  a vector), rows and columns (for a data frame), and so on.
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

with(airquality, subset(Ozone, Temp > 80))
}
\keyword{manip}
