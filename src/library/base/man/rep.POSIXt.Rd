\name{rep.POSIXt}
\alias{rep.POSIXct}
\alias{rep.POSIXlt}
\title{Replicate Elements of Date-Time objects}
\description{
  Methods for the generic function \code{rep} for date-time objects
}
\usage{
\method{rep}{POSIXct}(x, times, \dots)
}
\arguments{
  \item{x}{a \code{POSIXct} or \code{POSIXlt} object.}
  \item{times}{non-negative integer.  A vector giving the number of
    times to repeat each element if of length \code{length(x)}, or to
    repeat the whole vector if of length 1.}
  \item{\dots}{further arguments to be passed to or from other methods.}
}
\value{
  A vector of the same class as \code{x}.
}
\seealso{
  \code{\link{rep}}, \code{\link{DateTimeClasses}}
}
\examples{
x <- .leap.seconds[1:3]
rep(x, 2)
rep(as.POSIXlt(x), rep(2, 3))
}
\keyword{manip}
\keyword{chron}
