\name{rug}
\title{Add a Rug to a Plot}
\usage{
rug(x, ticksize=0.03, side=1, lwd=0.5)
}
\alias{rug}
\arguments{
 \item{x}{A numeric vector}
 \item{ticksize}{The length of the ticks making up the `rug'. Positive
   lengths give inwards ticks.}
 \item{side}{On which side of the plot box the rug will be
   plotted. Normally 1 (bottom) or 3 (top).}
 \item{lwd}{The line width of the ticks.}
}
\description{
    Adds a \emph{rug} representation (1-d plot) of the data to the plot.
}
\author{B.D. Ripley}
\seealso{\code{\link{jitter}} which you may want for ties in \code{x}.}
\examples{
data(faithful)
attach(faithful)
plot(density(eruptions, bw=0.15))
rug(eruptions)
}
\keyword{hplot}
