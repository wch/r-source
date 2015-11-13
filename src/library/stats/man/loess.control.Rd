% File src/library/stats/man/loess.control.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2015 R Core Team
% Distributed under GPL 2 or later

\name{loess.control}
\title{Set Parameters for Loess}
\usage{
loess.control(surface = c("interpolate", "direct"),
              statistics = c("approximate", "exact", "none"),
              trace.hat = c("exact", "approximate"),
              cell = 0.2, iterations = 4, iterTrace = FALSE, \dots)
}
\alias{loess.control}
\arguments{
 \item{surface}{should the fitted surface be computed exactly
   (\code{"direct"}) or via interpolation from a kd tree?  Can be abbreviated.}
 \item{statistics}{should the statistics be computed exactly,
   approximately or not at all?  Exact computation can be very slow.
   Can be abbreviated.}
 \item{trace.hat}{Only for the (default) case \code{(surface =
     "interpolate", statistics = "approximate")}: should the trace of
   the smoother matrix be computed exactly or approximately?  It is recommended to use the approximation
   for more than about 1000 data points.  Can be abbreviated.}
 \item{cell}{if interpolation is used this controls the accuracy of the
   approximation via the maximum number of points in a  cell in the kd
   tree. Cells with more than \code{floor(n*span*cell)} points are subdivided.}
 \item{iterations}{the number of iterations used in robust fitting,
   i.e. only if \code{family} is \code{"symmetric"}.}
 \item{iterTrace}{logical (or integer) determining if tracing
   information during the robust iterations (\code{iterations}\eqn{\ge
     2}{>= 2}) is produced.}
 \item{\dots}{further arguments which are ignored.}
}
\description{
  Set control parameters for \code{loess} fits.
}
\value{
  A list with components
  \item{surface}{}
  \item{statistics}{}
  \item{trace.hat}{}
  \item{cell}{}
  \item{iterations}{}
  \item{iterTrace}{}
  with meanings as explained under \sQuote{Arguments}.
}
\seealso{\code{\link{loess}}}
\keyword{smooth}
\keyword{loess}
