% File src/library/base/man/lengths.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2015 R Core Team
% Distributed under GPL 2 or later

\name{lengths}
\alias{lengths}
\title{Lengths of List Elements}
\description{
  Get the length of each element in a list as an integer or numeric vector.
}
\usage{
lengths(x)
}
\arguments{
  \item{x}{a list.}
}
\details{
  This function loops over \code{x} and returns a compatible vector
  containing the length of each element in \code{x}. Effectively,
  \code{length(x[[i]])} is called for all \code{i}, so any methods on
  \code{length} are considered.
}

\value{
  A non-negative \code{\link{integer}} of length \code{length(x)},
  except when any element has a length of more than
  \eqn{2^{31}-1}{2^31 - 1} elements, when it returns a double vector.
  The names are taken from the names on \code{x}, if any.
}

\seealso{\code{\link{length}} for getting the length of a vector.}
\examples{
require(stats)
## summarize by month
l <- split(airquality$Ozone, airquality$Month)
avgOz <- lapply(l, mean, na.rm=TRUE)
## merge result
airquality$avgOz <- rep(unlist(avgOz, use.names=FALSE), lengths(l))
## but this is safer and cleaner, but can be slower
airquality$avgOz <- unsplit(avgOz, airquality$Month)

## should always be true, except when a length does not fit in 32 bits
stopifnot(identical(lengths(l), vapply(l, length, integer(1L))))

## empty lists are not a problem
x <- list()
stopifnot(identical(lengths(x), integer()))

## and we should dispatch to length methods
f <- c(rep(1, 3), rep(2, 6), 3)
dates <- split(as.POSIXlt(Sys.time() + 1:10), f)
stopifnot(identical(lengths(dates), vapply(dates, length, integer(1L))))
}
\keyword{attribute}
