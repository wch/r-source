\name{plot.histogram}
\alias{plot.histogram}
\alias{lines.histogram}
\title{Plot Histograms}
\description{
  These are methods for objects of class \code{"histogram"}, typically
  produced by \code{\link{hist}}.
}
\usage{
\method{plot}{histogram}(x, freq = equidist, density = NULL, angle = 45,
               col = NULL, border = par("fg"), lty = NULL,
               main = paste("Histogram of", paste(x$xname, collapse="\n")),
               sub = NULL, xlab = x$xname, ylab,
               xlim = range(x$breaks), ylim = NULL,
               axes = TRUE, labels = FALSE, add = FALSE, \dots)

\method{lines}{histogram}(x, \dots)
}
\arguments{
  \item{x}{a \code{histogram} object, or a list with components
    \code{density}, \code{mid}, etc, see \code{\link{hist}} for
    information about the components of \code{x}.}
  \item{freq}{logical; if \code{TRUE}, the
    histogram graphic is to present a representation of frequencies, i.e,
    \code{x$counts}; if \code{FALSE}, \emph{relative} frequencies
    (\dQuote{probabilities}), i.e., \code{x$density}, are plotted.  The
    default is true for equidistant \code{breaks} and false otherwise.}
  \item{col}{a colour to be used to fill the bars.  The default of
    \code{NULL} yields unfilled bars.}
  \item{border}{the color of the border around the bars.}
  \item{angle, density}{select shading of bars by lines:
    see \code{\link{rect}}.}
  \item{lty}{the line type used for the bars, see also \code{\link{lines}}.}
  \item{main, sub, xlab, ylab}{these arguments to \code{title} have useful
    defaults here.}
  \item{xlim, ylim}{the range of x and y values with sensible defaults.}
  \item{axes}{logical, indicating if axes should be drawn.}
  \item{labels}{logical or character.  Additionally draw labels on top of bars,
    if not \code{FALSE}; if \code{TRUE}, draw the counts or rounded
    densities; if \code{labels} is a \code{character}, draw itself.}
  \item{add}{logical.  If \code{TRUE}, only the bars are added to the
    current plot.  This is what \code{lines.histogram(*)} does.}
  \item{\dots}{further graphical parameters to \code{title} and \code{axis}.}
}
\details{
  \code{lines.histogram(*)} is the same as
  \code{plot.histogram(*, add = TRUE)}.
}
\seealso{
  \code{\link{hist}},
  \code{\link{stem}},
  \code{\link{density}}.
}
\examples{
(wwt <- hist(women$weight, nc= 7, plot = FALSE))
plot(wwt, labels = TRUE) # default main & xlab using wwt$xname
plot(wwt, border = "dark blue", col = "light blue",
     main = "Histogram of 15 women's weights", xlab = "weight [pounds]")

## Fake "lines" example, using non-default labels:
w2 <- wwt; w2$counts <- w2$counts - 1
lines(w2, col = "Midnight Blue", labels = ifelse(w2$counts, "> 1", "1"))
}
\keyword{hplot}
\keyword{iplot}
