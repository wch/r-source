\name{plot.histogram}
\title{Plot Histograms}
\usage{
plot.histogram(x, freq = equidist, col = NULL, border = par("fg"),
               lty = NULL, main = paste("Histogram of", x$xname),
               xlim = range(x$breaks), ylim = range(y, 0),
               xlab = x$xname, ylab, axes = TRUE, labels = FALSE,
               add = FALSE, \dots)

lines.histogram(x, \dots)
}
\alias{plot.histogram}
\alias{lines.histogram}
\arguments{
  \item{x}{a \code{histogram} object, or a list with components
    \code{intensities}, \code{mid}, etc, see \code{\link{hist}}.}
  \item{freq}{logical; if \code{TRUE}, the
    histogram graphic is to present a representation of frequencies, i.e,
    \code{x$counts}; if \code{FALSE}, \emph{relative} frequencies
    (``probabilities''), i.e., \code{x$intensities}, are plotted.  The
    default is true for equidistant \code{breaks} and false otherwise.}
  \item{col}{a colour to be used to fill the bars.  The default of
    \code{NULL} yields unfilled bars.}
  \item{border}{the color of the border around the bars.}
  \item{lty}{the line type used for the bars, see also \code{\link{lines}}.}
  \item{xlim, ylim}{the range of x and y values with sensible defaults.}
  \item{main, xlab, ylab}{these arguments to \code{title} have useful
    defaults here.}
  \item{axes}{logical, indicating if axes should be drawn.}
  \item{labels}{logical or character.  Additionally draw labels on top of bars,
    if not \code{FALSE}; if \code{TRUE}, draw the counts or rounded
    intensities; if \code{labels} is a \code{character}, draw itself.}
  \item{add}{logical.  If \code{TRUE}, only the bars are added to the
    current plot.  This is what \code{lines.histogram(*)} does.}
  \item{\dots}{further graphical parameters to \code{title} and \code{axis}.}
}
\description{
  These are methods for objects of class \code{"histogram"}, typically
  produced by \code{\link{hist}(\dots)}.  See that page for information
  about the components of \code{x}.

  \code{lines.histogram(*)} is the same as
  \code{plot.histogram(*, add = TRUE)}.
}
\seealso{
  \code{\link{hist}},
  \code{\link{stem}},
  \code{\link{density}}.
}
\examples{
data(women)
str(wwt <- hist(women$weight, nc= 7, plot = FALSE))
plot(wwt, labels = TRUE) # default main & xlab using wwt$xname
plot(wwt, border = "dark blue", col = "light blue",
     main = "Histogram of 15 women's weights", xlab = "weight [pounds]")

## Fake "lines" example, using non-default labels:
w2 <- wwt; w2$counts <- w2$counts - 1
lines(w2, col = "Midnight Blue", labels = ifelse(w2$counts, "> 1", "1"))
}
\keyword{hplot}
\keyword{iplot}
