\name{rug}
\title{Add a Rug to a Plot}
\alias{rug}
\usage{
rug(x, ticksize=0.03, side=1, lwd=0.5, col,
    quiet = getOption("warn") < 0, \dots)
}
\arguments{
 \item{x}{A numeric vector}
 \item{ticksize}{The length of the ticks making up the \sQuote{rug}.
   Positive lengths give inwards ticks.}
 \item{side}{On which side of the plot box the rug will be
   plotted. Normally 1 (bottom) or 3 (top).}
 \item{lwd}{The line width of the ticks.}
 \item{col}{The colour the ticks are plotted in, default is black.}
 \item{quiet}{logical indicating if there should be a warning about
   clipped values.}
 \item{\dots}{further arguments, passed to \code{\link{axis}(...)}, such
   as \code{line} or \code{pos} for specifying the location of the rug.}
}
\description{
  Adds a \emph{rug} representation (1-d plot) of the data to the plot.
}
\details{
  Because of the way \code{rug} is implemented, only values of \code{x}
  that fall within the plot region are included.  There will be a
  warning if any finite values are omitted, but non-finite values are
  omitted silently.

  Because of the way colours are done the axis itself is coloured the
  same as the ticks.  You can always replot the box in black if you don't
  like this feature.
}
\references{
  Chambers, J. M. and Hastie, T. J. (1992)
  \emph{Statistical Models in S.}
  Wadsworth \& Brooks/Cole.
}
\seealso{\code{\link{jitter}} which you may want for ties in \code{x}.}
\examples{
require(stats)# both 'density' and its default method
with(faithful, {
    plot(density(eruptions, bw=0.15))
    rug(eruptions)
    rug(jitter(eruptions, amount = .01), side = 3, col = "light blue")
})
}
\keyword{aplot}
