\name{panel.smooth}
\title{Simple Panel Plot}
\usage{panel.smooth(x, y, col, pch, f=2/3, iter=3, \dots)}
\alias{panel.smooth}
\arguments{
 \item{x,y}{numeric vectors of the same length}
 \item{col,pch}{numeric or character codes for the color and point type;
   see \code{\link{par}}}
 \item{f, iter}{parameters for \code{\link{lowess}}, see there.}
 \item{\dots}{further arguments to \code{\link{lowess}}}
}
\description{
An example of a simple useful \code{panel} function to be used as
argument in e.g.,  \code{\link{coplot}} or \code{\link{pairs}}.
}
\seealso{\code{\link{coplot}(.)} and \code{\link{pairs}(.)} where
  \code{panel.smooth} is typically used; \code{\link{lowess}}.}
\examples{
data(swiss)
pairs(swiss, panel = panel.smooth, pch = ".")# emphasize the smooths
}
\keyword{hplot}
\keyword{dplot}

