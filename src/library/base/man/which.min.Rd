\name{which.min}
\alias{which.min}
\alias{which.max}
\title{Where is the Min() or Max() ?}
\concept{argmin}
\concept{argmax}
\concept{index of minimum}
\concept{index of maximum}
\description{
  Determines the location, i.e., index of the (first) minimum or maximum
  of a numeric vector.
}
\usage{
which.min(x)
which.max(x)
}
\arguments{
  \item{x}{numeric (integer or double) vector, whose
    \code{\link{min}} or \code{\link{max}} is searched for.}
}
\value{
  Missing and \code{NaN} values are discarded.
  
  an \code{\link{integer}} of length 1 or 0 (iff \code{x} has no
  non-\code{NA}s), giving the index of the \emph{first} minimum or
  maximum respectively of \code{x}.

  If this extremum is unique (or empty), the results are the same as
  (but more efficient than) \code{which(x == min(x))} or
  \code{which(x == max(x))} respectively.
}
\author{Martin Maechler}
\seealso{
  \code{\link{which}}, \code{\link{max.col}}, \code{\link{max}}, etc.

  \code{\link[nnet]{which.is.max}} in package \pkg{nnet} differs in
  breaking ties at random (and having a \dQuote{fuzz} in the definition
  of ties).
}
\examples{
x <- c(1:4,0:5,11)
which.min(x)
which.max(x)

## it *does* work with NA's present, by discarding them:
presidents[1:30]
range(presidents, na.rm = TRUE)
which.min(presidents) # 28
which.max(presidents) #  2
}
\keyword{utilities}
