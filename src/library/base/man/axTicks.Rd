\name{axTicks}
\alias{axTicks}
\title{Compute Axis Tickmark Locations}
\description{
  Compute tickmark locations, the same way as \R does internally.
  This is only non-trivial when \bold{log} coordinates are active.
  By default, gives the \code{at} values which
  \code{axis(side)} would use.
}
\usage{
axTicks(side, axp = NULL, usr = NULL, log = NULL)
}
\arguments{
  \item{side}{integer in 1:4, as for \code{\link{axis}}.}
  \item{axp}{numeric vector of length three, defaulting to
    \code{\link{par}("Zaxp")} where ``Z'' is ``x'' or ``y'' depending on
    the \code{side} argument.}
  \item{usr}{numeric vector of length four, defaulting to
    \code{\link{par}("usr")} giving horizontal (`x') and vertical (`y')
    user coordinate limits.}
  \item{log}{logical indicating if log coordinates are active; defaults to
    \code{\link{par}("Zlog") where `Z' is as for the \code{axp} argument
      above.}
}
\details{
  in the case of mainly
i.e. the \code{at} values which
  \code{axis(side)} would use by default.  In other words,
  \code{axis(side)} and \code{axis(side, at = axTicks(side))}
  should be identical.

  using par("Xaxp") , par("usr") & par("Xlog") where X = x|y
  an R version of internal CreateAtVector()

}
\value{
  numeric vector of coordinate values at which axis tickmarks can be
  drawn.  By default, when only the first argument is specified,
  these values should be identical to those that
  \code{\link{axis}(side)} would use or has used.
}
\seealso{\code{\link{axis}},\code{\link{par}}.}

\examples{
 plot(1:7, 10*21:27)
 axTicks(1)
 axTicks(2)
 stopifnot(identical(axTicks(1), axTicks(3)),
           identical(axTicks(2), axTicks(4)))

## Show how axTicks() and axis() correspond :
op <- par(mfrow = c(3,1))
for(x in 9999*c(1,2,8)) {
    plot(x,9, log = "x")
    cat(formatC(par("xaxp"),wid=5),";",T <- axTicks(1),"\n")
    rug(T, col="red")
}
par(op)
}
\keyword{dplot}

