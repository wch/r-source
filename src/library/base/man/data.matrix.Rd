\name{data.matrix}
\alias{data.matrix}
\title{Data Frame to Numeric Matrix}
\description{
  Return the matrix obtained by converting all the variables in a data
  frame to numeric mode and then binding them together as the columns of
  a matrix.  Factors and ordered factors are replaced by their internal
  codes.
}
\usage{
data.matrix(frame, rownames.force = FALSE)
}
\arguments{
  \item{frame}{a data frame whose components are logical vectors,
    factors or numeric vectors.}
  \item{rownames.force}{logical indicating if the resulting matrix must
    have non-NULL \code{\link{rownames}}.  The default, \code{FALSE},
    leads to memory-saving behavior \emph{different} than in \R versions
    2.4.x and earlier.}
}
\details{
  Supplying a data frame with columns which are not numeric, factor or logical
  is an error.  A warning is given if any non-factor column has a class,
  as then information can be lost.
}
\value{
  If \code{frame} inherits from class \code{"data.frame"}, an integer or
  numeric matrix of the same dimensions as \code{frame}, with dimnames
  taken from the \code{row.names} and \code{names}.  When the row.names
  are \dQuote{trivial}, i.e., equivalent to \code{1:nrow(.)}, the
  resulting matrix gets \code{\link{NULL}} \code{dimnames[[1]]} unless
  \code{rownames.force} is true.

  Otherwise, the result of \code{\link{as.matrix}}.
}
\seealso{
  \code{\link{as.matrix}},
  \code{\link{data.frame}},
  \code{\link{matrix}}.
}
\references{
  Chambers, J. M. (1992)
  \emph{Data for models.}
  Chapter 3 of \emph{Statistical Models in S}
  eds J. M. Chambers and T. J. Hastie, Wadsworth \& Brooks/Cole.
}
\examples{
DF <- data.frame(a=1:3, b=letters[10:12],
                 c=seq(as.Date("2004-01-01"), by = "week", len = 3),
                 stringsAsFactors = TRUE)
data.matrix(DF[1:2])
data.matrix(DF) # gives a warning and quotes dates as #days since 1970.
}
\keyword{array}
