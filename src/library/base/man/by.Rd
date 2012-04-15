% File src/library/base/man/by.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2011 R Core Team
% Distributed under GPL 2 or later

\name{by}
\alias{by}
\alias{by.default}
\alias{by.data.frame}
\alias{print.by}
\title{Apply a Function to a Data Frame Split by Factors}
\description{
  Function \code{by} is an object-oriented wrapper for
  \code{\link{tapply}} applied to data frames.
}
\usage{
by(data, INDICES, FUN, \dots, simplify = TRUE)
}
\arguments{
 \item{data}{an \R object, normally a data frame, possibly a matrix.}
 \item{INDICES}{a factor or a list of factors, each of length
   \code{nrow(data)}.}
 \item{FUN}{a function to be applied to data frame subsets of \code{data}.}
 \item{\dots}{further arguments to \code{FUN}.}
 \item{simplify}{logical: see \code{\link{tapply}}.}
}
\details{
  A data frame is split by row into data frames
  subsetted by the values of one or more factors, and function
  \code{FUN} is applied to each subset in turn.

  Object \code{data} will be coerced to a data frame by the default
  method, \emph{but} if this results in a 1-column data frame, the
  objects passed to \code{FUN} are dropped to a subsets of that column.
}
\value{
  An object of class \code{"by"}, giving the results for each subset.
  This is always a list if \code{simplify} is false, otherwise a list or
  array (see \code{\link{tapply}}).
}
\seealso{\code{\link{tapply}}, \code{\link{simplify2array}}.
  \code{\link{ave}} also applies a function block-wise.
}
\examples{
require(stats)
by(warpbreaks[, 1:2], warpbreaks[,"tension"], summary)
by(warpbreaks[, 1],   warpbreaks[, -1],       summary)
by(warpbreaks, warpbreaks[,"tension"],
   function(x) lm(breaks ~ wool, data = x))

## now suppose we want to extract the coefficients by group
tmp <- with(warpbreaks,
            by(warpbreaks, tension,
               function(x) lm(breaks ~ wool, data = x)))
sapply(tmp, coef)
}
\keyword{iteration}
\keyword{category}
