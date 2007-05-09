\name{points}
\alias{points}
\alias{points.default}
\title{Add Points to a Plot}
\description{
  \code{points} is a generic function to draw a sequence of points at
  the specified coordinates.  The specified character(s) are plotted,
  centered at the coordinates.
}
\usage{
points(x, \dots)

\method{points}{default}(x, y = NULL, type = "p", \dots)
}
\arguments{
  \item{x, y}{coordinate vectors of points to plot.}
  \item{type}{character indicating the type of plotting; actually any of
    the \code{type}s as in \code{\link{plot.default}}.}
  \item{\dots}{Further graphical parameters may also be supplied as
    arguments.  See Details.}
}
\details{
  The coordinates can be passed in a plotting structure
  (a list with \code{x} and \code{y} components), a two-column matrix, a
  time series, \dots.  See \code{\link{xy.coords}}.

  Graphical parameters commonly used are
  \describe{
    \item{\code{pch}}{plotting \dQuote{character}, i.e., symbol to use.  This can
      either be a single character or an integer code for one of a set of
      graphics symbols.  The full set of S symbols is available with
      \code{pch=0:18}, see the last picture from \code{example(points)},
      i.e., the examples below.
      % (currently, \R uses circles instead of octagons).

      In addition, there is a special set of \R plotting symbols which
      can be obtained with \code{pch=19:25} and \code{21:25} can be
      colored and filled with different colors:
      \itemize{
	\item \code{pch=19}: solid circle,
	\item \code{pch=20}: bullet (smaller circle),
	\item \code{pch=21}: circle,
	\item \code{pch=22}: square,
	\item \code{pch=23}: diamond,
	\item \code{pch=24}: triangle point-up,
	\item \code{pch=25}: triangle point down.
      }
      Values \code{pch=26:32} are currently unused, and \code{pch=32:255}
      give the text symbol in a single-byte locale.  In a multi-byte locale
      such as UTF-8, numeric values of \code{pch} greater than or equal to
      32 specify a Unicode code point (except for the symbol font as
      selected by \code{\link{par}(font = 5)}).
      
      If \code{pch} is an integer or character \code{NA} or an empty
      character string, the point is omitted from the plot.
      
      Value \code{pch="."} is handled specially.  It is a rectangle of
      side 0.01 inch (scaled by \code{cex}).  In addition, if \code{cex =
	1} (the default), each side is at least one pixel (1/72 inch on
      the \code{\link{pdf}}, \code{\link{postscript}} and
      \code{\link{xfig}} devices).
    }
    \item{\code{col}}{color code or name, see \code{\link{par}}.}
    \item{\code{bg}}{background (\dQuote{fill}) color for the open plot
      symbols given by \code{pch=21:25}.}
    \item{\code{cex}}{character (or symbol) expansion: a numerical vector.
      This works as a multiple of \code{\link{par}("cex")}.}
    \item{\code{lwd}}{line width for drawing symbols see \code{\link{par}}.}
  }
  Others less commonly used are \code{lty} and \code{lwd} for
  types such as \code{"b"} and \code{"l"}.
  
  Graphical parameters \code{pch}, \code{col}, \code{bg}, \code{cex} and
  \code{lwd} can be vectors (which will be recycled as needed) giving a
  value for each point plotted.  If lines are to be plotted (e.g. for
  \code{type = "b"} the first element of \code{lwd} is used.

  Points whose \code{x}, \code{y}, \code{pch}, \code{col} or \code{cex}
  value is \code{NA} are omitted from the plot.
}
\note{
  What is meant by \sQuote{a single character} is locale-dependent.

  The encoding may not have symbols for some or all of the characters in
  \code{pch=128:255} 
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{plot}}, \code{\link{lines}}, and the underlying
  workhorse function \code{\link{plot.xy}}.
}
\examples{
require(stats) # for rnorm
plot(-4:4, -4:4, type = "n")# setting up coord. system
points(rnorm(200), rnorm(200), col = "red")
points(rnorm(100)/2, rnorm(100)/2, col = "blue", cex = 1.5)

op <- par(bg = "light blue")
x <- seq(0,2*pi, len=51)
## something "between type='b' and type='o'":
plot(x, sin(x), type="o", pch=21, bg=par("bg"), col = "blue", cex=.6,
 main='plot(..., type="o", pch=21, bg=par("bg"))')
par(op)

##-------- Showing all the extra & some char graphics symbols ------------
Pex <- 3 ## good for both .Device=="postscript" and "x11"
ipch <- 0:35; np <- length(ipch); k <- floor(sqrt(np)); dd <- c(-1,1)/2
rx <- dd + range(ix <- ipch \%/\% k)
ry <- dd + range(iy <- 3 + (k-1)- ipch \%\% k)
pch <- as.list(ipch)
pch[26+ 1:10] <- as.list(c("*",".", "o","O","0","+","-","|","\%","#"))
plot(rx, ry, type="n", axes = FALSE, xlab = "", ylab = "",
     main = paste("plot symbols :  points (...  pch = *, cex =", Pex,")"))
abline(v = ix, h = iy, col = "lightgray", lty = "dotted")
for(i in 1:np) {
  pc <- pch[[i]]
  points(ix[i], iy[i], pch = pc, col = "red", bg = "yellow", cex = Pex)
  ## red symbols with a yellow interior (where available)
  text(ix[i] - .3, iy[i], pc, col = "brown", cex = 1.2)
}
}
\keyword{aplot}
