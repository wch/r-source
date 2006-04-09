\name{by}
\alias{by}
\alias{by.default}
\alias{by.data.frame}
\alias{print.by}
\title{Apply a Function to a Data Frame split by Factors}
\description{
  Function \code{by} is an object-oriented wrapper for
  \code{\link{tapply}} applied to data frames.
}
\usage{
by(data, INDICES, FUN, \dots)
}
\arguments{
 \item{data}{an \R object, normally a data frame, possibly a matrix.}
 \item{INDICES}{a factor or a list of factors, each of length
   \code{nrow(data)}.}
 \item{FUN}{a function to be applied to data frame subsets of \code{data}.}
 \item{\dots}{further arguments to \code{FUN}.}
}
\details{
  A data frame is split by row into data frames
  subsetted by the values of one or more factors, and function
  \code{FUN} is applied to each subset in turn.

  Object \code{data} will be coerced to a data frame by default.
}
\value{
  A list of class \code{"by"}, giving the results for each subset.
}
\seealso{\code{\link{tapply}}}

\examples{
require(stats)
attach(warpbreaks)
by(warpbreaks[, 1:2], tension, summary)
by(warpbreaks[, 1], list(wool = wool, tension = tension), summary)
by(warpbreaks, tension, function(x) lm(breaks ~ wool, data = x))

## now suppose we want to extract the coefficients by group
tmp <- by(warpbreaks, tension, function(x) lm(breaks ~ wool, data = x))
sapply(tmp, coef)

detach("warpbreaks")
}
\keyword{iteration}
\keyword{category}
