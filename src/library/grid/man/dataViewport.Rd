\name{dataViewport}
\alias{dataViewport}
\title{ Create a Viewport with Scales based on Data }
\description{
  This is a convenience function for producing a viewport with
  x- and/or y-scales based on numeric values passed to the function.
}
\usage{
dataViewport(xData = NULL, yData = NULL, xscale = NULL,
             yscale = NULL, extension = 0.05, ...)
}
\arguments{
  \item{xData}{ A numeric vector of data. }
  \item{yData}{ A numeric vector of data. }
  \item{xscale}{ A numeric vector (length 2). }
  \item{yscale}{ A numeric vector (length 2). }
  \item{extension}{ A numeric. }
  \item{\dots}{ All other arguments will be passed to a call to
    the \code{viewport()} function. }
}
\details{
  If \code{xscale} is not specified then the values in \code{x} are
  used to generate an x-scale based on the range of \code{x}, extended
  by the proportion specified in \code{extension}.  Similarly for the
  y-scale.
}
\value{
  A grid viewport object.
}
\author{ Paul Murrell }
\seealso{
  \code{\link{viewport}} and
  \code{\link{plotViewport}}.
}
\keyword{dplot}
