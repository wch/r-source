\name{panel.smooth}
\alias{panel.smooth}
\title{Simple Panel Plot}
\description{
  An example of a simple useful \code{panel} function to be used as
  argument in e.g., \code{\link{coplot}} or \code{\link{pairs}}.
}
\usage{
panel.smooth(x, y, col = par("col"), bg = NA, pch = par("pch"), cex = 1,
             col.smooth = "red", span = 2/3, iter=3, \dots)
}
\arguments{
  \item{x,y}{numeric vectors of the same length}
  \item{col,bg,pch,cex}{numeric or character codes for the color(s),
    point type and size of \code{\link{points}}; see also
    \code{\link{par}}.}
  \item{col.smooth}{color to be used by \code{lines} for drawing the
    smooths.}
  \item{span}{smoothing parameter \code{f} for \code{\link{lowess}}, see
    there.}
  \item{iter}{number of robustness iterations for \code{\link{lowess}}.}
  \item{\dots}{further arguments to \code{\link{lines}}.}
}
\seealso{
  \code{\link{coplot}} and \code{\link{pairs}} where \code{panel.smooth}
  is typically used;
  \code{\link{lowess}}.
}
\examples{
pairs(swiss, panel = panel.smooth, pch = ".")# emphasize the smooths
pairs(swiss, panel = panel.smooth, lwd = 2, cex= 1.5, col="blue")# hmm...
}
\keyword{hplot}
\keyword{dplot}
