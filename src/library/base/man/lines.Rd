\name{lines}
\title{Add Connected Line Segments to a Plot}
\usage{
lines(x, \dots)
lines.default(x, y=NULL, type="l", col=par("col"), \dots)
}
\alias{lines}
\alias{lines.default}
\arguments{
\item{x, y}{coordinate vectors of points to join.}
\item{type}{character indicating the type of plotting; actually any of
  the \code{type}s as in \code{\link{plot}(..)}.}
\item{col}{color to use.}
\item{\dots}{Further graphical parameters (see \code{\link{par}}) may
  also be supplied as arguments, particularly, line type, \code{lty} and
  line width, \code{lwd}.}
}
\description{
\code{lines} is a generic function taking the coordinates given in the \code{x}
and \code{y} vectors and joining the corresponding points with line segments.
The coordinates can alternatively be passed to \code{lines}
in a plotting structure (a list with x and y components),
a time series, etc.
}
\seealso{
\code{\link{points}}, \code{\link{plot}}, and the underlying ``primitive''
\code{\link{plot.xy}}.
}
\examples{
data(cars)
# draw a smooth line through a scatter plot
plot(cars, main="Stopping Distance versus Speed")
lines(lowess(cars))
}
\keyword{aplot}
