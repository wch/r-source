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
data.matrix(frame)
}
\arguments{
  \item{frame}{a data frame whose components are logical vectors,
    factors or numeric vectors.}
}
\details{
  Supplying a data frame with columns which are not numeric, factor or logical
  is an error.  A warning is given if any non-factor column has a class,
  as then information can be lost.
}
\value{
  If \code{frame} is a data frame, a numeric matrix of the same
  dimensions as \code{frame}, with dimnames taken from the row.names and
  names.

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
