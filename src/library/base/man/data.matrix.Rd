\name{data.matrix}
\alias{data.matrix}
\title{Data Frame to Numeric Matrix}
\description{
  Return the matrix obtained by converting all the variables in a data
  frame to numeric mode and then binding them together as the columns of
  a matrix.  Factors and ordered factors are replaced by their codes.
}
\usage{
data.matrix(frame)
}
\arguments{
  \item{frame}{a data frame whose components are logical vectors,
    factors or numeric vectors.}
}
\seealso{
  \code{\link{as.matrix}},
  \code{\link{codes}},
  \code{\link{data.frame}},
  \code{\link{matrix}}.
}
\keyword{array}
