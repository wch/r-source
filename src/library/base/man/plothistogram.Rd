\name{plot.histogram}
\title{Plot Histograms}
\usage{
plot.histogram(x, freq = NULL, col = NULL, border = par("fg"),
	 main = paste("Histogram of", x$xname),
	 xlim = range(x$breaks), ylim = range(y, 0),
	 xlab = x$xname, ylab, axes = TRUE, labels = FALSE, \dots)

lines.histogram(x, freq = NULL, col = NULL, border = par("fg"),
                lty = NULL, \dots)
}
\alias{plot.histogram}
\alias{lines.histogram}
\arguments{
  \item{x}{a \code{histogram} object, or a list with components
    \code{intensities}, \code{mid}, etc, see \code{\link{hist}}.}
  \item{freq}{logical; if \code{TRUE}, the
    histogram graphic is to present a representation of frequencies, i.e,
    \code{x$counts}; if \code{FALSE},
    \emph{relative} frequencies (``probabilities''), i.e., \code{x$intensities},
    are plotted.}
  \item{col}{a colour to be used to fill the bars.
    The default of \code{NULL} yields unfilled bars.}
  \item{border}{the color of the border around the bars.}
  \item{main, xlab, ylab, axes}{these arguments to \code{title} have useful
    defaults here.}
  \item{xlim, ylim}{the range of x and y values with sensible defaults.}
  \item{lty}{the line type, see also \code{\link{lines}}.}
  \item{labels}{logical.  Additionally draw labels on top of bars,
    if \code{TRUE}.}
  \item{\dots}{further graphical parameters to \code{title} and \code{axis}.}
}
\description{
  These are methods for objects of class \code{"histogram"}, typically
  produced by \code{\link{hist}(\dots)}.  See that page for
  information about the components of \code{x}.
}
\seealso{\code{\link{hist}}, \code{\link{stem}}, \code{\link{density}}.}
\examples{
data(women)
str(wwt <- hist(women$weight, nc= 7, plot = FALSE))
plot(wwt) # default main & xlab using wwt$xname
plot(wwt, border = "dark blue", col = "light blue",
     main = "Histogram of 15 women's weights", xlab = "weight [pounds]")
## Fake "lines" example:
w2 <- wwt; w2$counts <- w2$counts - 1
lines(w2, col = "Midnight Blue")
}
\keyword{hplot}
\keyword{iplot}
