\name{data.matrix}
\alias{data.matrix}
\title{Convert a Data Frame to a Numeric Matrix}
\description{
  Return the matrix obtained by converting all the variables in a data
  frame to numeric mode and then binding them together as the columns of
  a matrix.  Factors and ordered factors are replaced by their internal
  codes.
}
\usage{
data.matrix(frame, rownames.force = NA)
}
\arguments{
  \item{frame}{a data frame whose components are logical vectors,
    factors or numeric vectors.}
  \item{rownames.force}{logical indicating if the resulting matrix
    should have character (rather than \code{NULL})
    \code{\link{rownames}}.  The default, \code{NA}, uses \code{NULL}
    rownames if the data frame has \sQuote{automatic} row.names or for a
    zero-row data frame.}
}
\details{
  Supplying a data frame with columns which are not numeric, factor or logical
  is an error.  A warning is given if any non-factor column has a class,
  as then information can be lost.
}
\value{
  If \code{frame} inherits from class \code{"data.frame"}, an integer or
  numeric matrix of the same dimensions as \code{frame}, with dimnames
  taken from the \code{row.names} (or \code{NULL}, depending on
  \code{rownames.force}) and \code{names}.

  Otherwise, the result of \code{\link{as.matrix}}.
}
\note{
  The default behaviour for data frames differs from \R < 2.5.0 which
  always gave the result character rownames.
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
