% File src/library/stats/man/loess.control.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2011 R Core Team
% Distributed under GPL 2 or later

\name{loess.control}
\title{Set Parameters for Loess}
\usage{
loess.control(surface = c("interpolate", "direct"),
              statistics = c("approximate", "exact"),
              trace.hat = c("exact", "approximate"),
              cell = 0.2, iterations = 4, \dots)
}
\alias{loess.control}
\arguments{
 \item{surface}{should the fitted surface be computed exactly or via
   interpolation from a kd tree?  Can be abbreviated.}
 \item{statistics}{should the statistics be computed exactly or
   approximately? Exact computation can be very slow.  Can be abbreviated.}
 \item{trace.hat}{should the trace of the smoother matrix be computed
   exactly or approximately? It is recommended to use the approximation
   for more than about 1000 data points.  Can be abbreviated.}
 \item{cell}{if interpolation is used this controls the accuracy of the
   approximation via the maximum number of points in a  cell in the kd
   tree. Cells with more than \code{floor(n*span*cell)} points are subdivided.}
 \item{iterations}{the number of iterations used in robust fitting.}
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
  with meanings as explained under \sQuote{Arguments}.
}
\seealso{\code{\link{loess}}}
\keyword{smooth}
\keyword{loess}
