\name{panel.smooth}
\title{Simple Panel Plot}
\usage{panel.smooth(x, y, col, bg=NA, pch, cex = 1, span = 2/3, iter=3, \dots)}
\alias{panel.smooth}
\arguments{
  \item{x,y}{numeric vectors of the same length}
  \item{col,bg,pch,cex}{numeric or character codes for the color(s), point
    type and size of \code{\link{points}}; see also \code{\link{par}}.}
  \item{span}{smoothing parameter \code{f} for \code{\link{lowess}}, see there.}
  \item{iter}{number of robustness iterations for \code{\link{lowess}}.}
  \item{\dots}{further arguments to \code{\link{lines}}.}
}
\description{
  An example of a simple useful \code{panel} function to be used as
  argument in e.g., \code{\link{coplot}} or \code{\link{pairs}}.
}
\seealso{\code{\link{coplot}(.)} and \code{\link{pairs}(.)} where
  \code{panel.smooth} is typically used; \code{\link{lowess}}.}
\examples{
data(swiss)
pairs(swiss, panel = panel.smooth, pch = ".")# emphasize the smooths
pairs(swiss, panel = panel.smooth, lwd = 2, cex= 1.5, col="blue")# hmm..
}
\keyword{hplot}
\keyword{dplot}

