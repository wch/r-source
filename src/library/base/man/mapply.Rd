\name{mapply}
\alias{mapply}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Apply a function to multiple list or vector arguments }
\description{
  A multivariate version of \code{\link{sapply}}.
  \code{mapply} applies \code{FUN} to the first elements of each \dots
  argument, the second elements, the third elements, and so on.
  Arguments are recycled if necessary. 
}
\usage{
mapply(FUN, ..., MoreArgs = NULL, SIMPLIFY = TRUE, USE.NAMES = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{FUN}{Function to apply }
  \item{\dots}{Arguments to vectorise over (list or vector) }
  \item{MoreArgs}{A list of other arguments to \code{FUN} }
  \item{SIMPLIFY}{Attempt to reduce the result to a vector or matrix? }
  \item{USE.NAMES}{If the first \dots argument is character and the
    result doesn't already have names, use it as the names}
}
\value{
  A list, vector, or matrix.
}

\seealso{\code{\link{sapply}}}, \code{\link{outer}} }
\examples{
 mapply(rep, 1:4, 4:1)

 mapply(rep, times=1:4, x=4:1)

 mapply(rep, times=1:4, MoreArgs=list(x=42))

}
\keyword{manip }% at least one, from doc/KEYWORDS
\keyword{utilities}% __ONLY ONE__ keyword per line
