\name{plot.xy}
\title{Basic Internal Plot Function}
\usage{
plot.xy(xy, type, pch=1, lty="solid", col=par("fg"), bg=NA, cex=1, \dots)
}
\alias{plot.xy}
\arguments{
\item{xy}{A four-element list as results from \code{\link{xy.coords}(..)}.}
\item{type}{1 character code.}
\item{pch}{character or integer code for kind of points/lines, see
  \code{\link{points.default}}.}
\item{lty}{line type code, see \code{\link{lines}}.}
\item{col}{color code or name, see \code{\link{colors}}, \code{\link{palette}}.}
\item{bg}{background (``fill'') color for open plot symbols.}
\item{cex}{character expansion}
\item{\dots}{further graphical parameters}
}
\description{
  This is \bold{the} internal function that does the basic plotting of points
  and lines.
  Usually, one should rather use the higher level functions instead
  and refer to their help pages for explanation of the arguments.
}
\seealso{
\code{\link{plot}}, \code{\link{plot.default}}, \code{\link{points}},
\code{\link{lines}}.
}
\examples{
points.default # to see how it calls "plot.xy(xy.coords(x, y), ....)"
}
\keyword{aplot}
