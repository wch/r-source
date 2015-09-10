% File src/library/base/man/which.min.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2015 R Core Team
% Distributed under GPL 2 or later

\name{which.min}
\alias{which.min}
\alias{which.max}
\title{Where is the Min() or Max() or first TRUE or FALSE ?}
\concept{argmin}
\concept{argmax}
\concept{index of minimum}
\concept{index of maximum}
\concept{index of first TRUE}
\concept{index of first FALSE}
\description{
  Determines the location, i.e., index of the (first) minimum or maximum
  of a numeric (or logical) vector.
}
\usage{
which.min(x)
which.max(x)
}
\arguments{
  \item{x}{numeric (integer or double) vector, whose
    \code{\link{min}} or \code{\link{max}} is searched for.}
}
\note{
  For a \code{\link{logical}} vector \code{x} with both \code{FALSE} and
  \code{TRUE} values, \code{which.min(x)} and \code{which.max(x)} return
  the index of the first \code{FALSE} or \code{TRUE}, respectively, as
  \code{FALSE < TRUE}.
}
\value{
  Missing and \code{NaN} values are discarded.

  an \code{\link{integer}} of length 1 or 0 (iff \code{x} has no
  non-\code{NA}s), giving the index of the \emph{first} minimum or
  maximum respectively of \code{x}.

  If this extremum is unique (or empty), the results are the same as
  (but more efficient than) \code{which(x == min(x, na.rm = TRUE))} or
  \code{which(x == max(x, na.rm = TRUE))} respectively.
}
\author{Martin Maechler}
\seealso{
  \code{\link{which}}, \code{\link{max.col}}, \code{\link{max}}, etc.

  Use \code{\link{arrayInd}()}, if you need array/matrix indices instead
  of 1D vector ones.

  \code{\link[nnet]{which.is.max}} in package \CRANpkg{nnet} differs in
  breaking ties at random (and having a \sQuote{fuzz} in the definition
  of ties).
}
\examples{
x <- c(1:4, 0:5, 11)
which.min(x)
which.max(x)

## it *does* work with NA's present, by discarding them:
presidents[1:30]
range(presidents, na.rm = TRUE)
which.min(presidents) # 28
which.max(presidents) #  2

## Find the first occurrence, i.e. the first TRUE, if there is at least one:
x <- rpois(10000, lambda = 10); x[sample.int(50, 20)] <- NA
## where is the first value >= 20 ?
which.max(x >= 20)
}
\keyword{utilities}
