\name{plot.xy}
\title{Basic Internal Plot Function}
\usage{
plot.xy(xy, type, pch=1, lty="solid", col=par("fg"), bg=NA, cex=1, \dots)
}
\alias{plot.xy}
\arguments{
\item{xy}{A four-element list as results from \code{\link{xy.coords}(..)}.}
\item{type}{1 character code}
\item{pch}{character or integer code for kind of points}
\item{lty}{line type code}
\item{col}{color code}
\item{bg}{background color}
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
\code{\link{plot}}, \code{\link{plot.default}}, \code{\link{points}}, \code{\link{lines}}.
}
\examples{
points.default # just calling "plot.xy(xy.coords(x, y), type = "p", ......)"
}
\keyword{aplot}
