\name{points}
\title{Add Points to a Plot}
\usage{
points(x, \dots)
points.default(x, y=NULL, type="p", pch=1, col="black", bg=NA, cex=1, \dots)
}
\alias{points}
\alias{points.default}
\arguments{
\item{x, y}{coordinate vectors of points to plot.}
\item{type}{character indicating the type of plotting; actually any of
  the \code{type}s as in \code{\link{plot}(..)}.}
\item{pch}{plotting `character', i.e. symbol to use.  \code{pch} can
  either be a \code{\link{character}} or an integer code for a set of
  graphics symbols.  The full set of S symbols is available with
  \code{pch=0:18}.

  In addition, there is a special set of \R plotting symbols which
  can be obtained with \code{pch=19:25} and can be colored and filled
  with different colors:
  \itemize{
    \item\code{pch=19}: solid circle,
    \item\code{pch=20}: bullet,
    \item\code{pch=21}: circle,
    \item\code{pch=22}: square,
    \item\code{pch=23}: diamond,
    \item\code{pch=24}: triangle point-up,
    \item\code{pch=25}: triangle point down.
  }
}
\item{col}{color code or name, see \code{\link{colors}}, \code{\link{palette}}.}
\item{bg}{background (``fill'') color for open plot symbols}
\item{cex}{character expansion}
\item{\dots}{Further graphical parameters (see \code{\link{plot.xy}} and
  \code{\link{par}}) may also be supplied as arguments.}
}
\description{
\code{points} is a generic function to draw a sequence of points at the
specified coordinates.
The specified character(s) are plotted, centered at the coordinates.

Graphical parameters are permitted as arguments to this function.
}
\seealso{
\code{\link{plot}}, \code{\link{lines}}, and the underlying ``primitive''
\code{\link{plot.xy}}.
}
\examples{
plot(-4:4, -4:4, type = "n")# setting up coord. system
points(rnorm(200), rnorm(200), col = "red")
points(rnorm(100)/2, rnorm(100)/2, col = "blue", cex = 1.5)

##-------- Showing all the graphics symbols ------------
Pex <- if(.Device=="postscript") 9 else 20
ipch <- 1:25; dd <- c(-1,1)/2
rx <- dd+ range(ix <- (ipch-1) \%/\% 5)
ry <- dd+ range(iy <- 3 + 4-(ipch-1) \%\% 5)
plot(rx, ry, type="n", axes = F, xlab = "", ylab = "",
     main = "plot symbols :  points (..  pch = * )")
abline(v=ix, h=iy, col = "lightgray", lty = "dotted")
for(i in ipch) { # red symbols with a yellow interior (where available)
  points(ix[i],      iy[i], pch=i, col="red", bg="yellow", cex = Pex)
  text  (ix[i] - .3, iy[i], i, col="brown", cex = 1.5)
}
}
\keyword{aplot}
