\name{bxp}
\title{Box Plots from Summaries}
\usage{
bxp(z, notch = FALSE, width = NULL, varwidth = FALSE, notch.frac = 0.5,
    boxwex = 0.8, border = par("fg"), col = NULL, log = "",
    pars = NULL, frame.plot = axes, \dots)
}
\alias{bxp}
\arguments{
\item{z}{a list containing data summaries to be used in constructing the plots.
  These are usually the result of a call to \code{\link{boxplot}},
  but can be generated in any fashion.}
\item{notch}{if \code{notch} is \code{TRUE}, a notch is drawn in each
  side of the boxes.  If the notches of two plots do not
  overlap then the medians are significantly different at the
  5 percent level.}
\item{width}{a vector giving the relative widths of the boxes making up
  the plot.}
\item{varwidth}{if \code{varwidth} is \code{TRUE}, the boxes are drawn
  with widths proportional to the square-roots of the number of
  observations in the groups.}
\item{boxwex}{a scale factor to be applied to all boxes.  When there
  are only a few groups, the appearance of the plot can be improved
  by making the boxes narrower.}
\item{notch.frac}{numeric in (0,1). When \code{notch=TRUE}, the fraction
  of the box width that the notches should use.}
\item{border}{character, the color of the box borders. Is recycled for
  multiple boxes.}
\item{col}{character; the color within the box. Is recycled for
  multiple boxes}
\item{log}{character, indicating if any axis should be drawn in
  logarithmic scale, as in \code{\link{plot.default}}.}
\item{frame.plot}{logical, indicating if a ``frame''
  (\code{\link{box}()}) should be drawn; defaults to \code{TRUE}, unless
  \code{axes = FALSE} is specified.}
\item{pars,\dots}{Graphical parameters can be passed as arguments to
  this function, either as a list (\code{pars}) or normally(\code{\dots}).}
}
\description{
  \code{bxp(..)} draws box plots based on the given summaries in
  \code{z}.  It is usually called from within \code{\link{boxplot}(..)},
  but can be invoked directly.
}
\value{An invisible vector with the x-coordinates of box centers, useful
  for adding to the plot.
}
\examples{
str(bx.p <- boxplot(split(rnorm(100), gl(5,20))))
op <- par(mfrow= c(2,2))
bxp(bx.p, xaxt = "n")
bxp(bx.p, notch = TRUE, axes = FALSE)
bxp(bx.p, notch = TRUE, col= 'lightblue', frame = FALSE)
bxp(bx.p, notch = TRUE, col= 'lightblue', border='red',
    log = "x", ylim = c(-1,4), main = "bxp(*, log='x', ylim=*)")
par(op)
}
\keyword{aplot}
