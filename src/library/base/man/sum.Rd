\name{sum}
\alias{sum}
\title{Sum of Vector Elements}
\description{
  \code{sum} returns the sum of all the values
  present in its arguments.
}
\usage{
sum(\dots, na.rm = FALSE)
}
\arguments{
  \item{\dots}{numeric or complex or logical vectors.}
  \item{na.rm}{logical.  Should missing values be removed?}
}
\value{
  The sum. If all of \code{\dots} are of type integer or logical, then
  the sum is integer, and in that case the result will be \code{NA} (with a
  warning) if integer overflow occurs.  Otherwise it is a length-one
  numeric or complex vector.

  \strong{NB:} the sum of an empty set is zero, by definition.
}
\details{
  This is a generic function: methods can be defined for it
  directly or via the \code{\link[base:groupGeneric]{Summary}} group generic.
  For this to work properly, the arguments \code{\dots} should be
  unnamed, and dispatch is on the first argument.

  If \code{na.rm} is \code{FALSE} an \code{NA}
  value in any of the arguments will cause
  a value of \code{NA} to be returned, otherwise
  \code{NA} values are ignored.

  Logical true values are regarded as one, false values as zero.
  For historical reasons, \code{NULL} is accepted and treated as if it
  were \code{integer(0)}.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\keyword{arith}
