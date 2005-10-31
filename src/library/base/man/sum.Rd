\name{sum}
\title{Sum of Vector Elements}
\usage{
sum(\dots, na.rm = FALSE)
}
\alias{sum}
\description{
  \code{sum} returns the sum of all the values
  present in its arguments.
}
\arguments{
  \item{\dots}{numeric or complex or logical vectors.}
  \item{na.rm}{logical. Should missing values be removed?}
}
\value{
  The sum. If all of \code{\dots} are of type integer or logical, then
  the sum is integer, and in that case the result will be \code{NA} (with a
  warning) if integer overflow occurs.

  \strong{NB:} the sum of an empty set is (integer) zero, by definition.
  }
\details{
  This is a generic function: methods can be defined for it
  directly or via the \code{\link{Summary}} group generic.

  If \code{na.rm} is \code{FALSE} an \code{NA}
  value in any of the arguments will cause
  a value of \code{NA} to be returned, otherwise
  \code{NA} values are ignored.

  Logical true values are regarded as one, false values as zero.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\keyword{arith}
