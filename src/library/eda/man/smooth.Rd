\name{smooth}
\title{Median Smoothing}
\usage{
smooth(x)
}
\arguments{
\item{x}{a vector or time series}
}
\description{
    This function applies the smoothing method \emph{3RSR}
    to \code{x}.  \emph{3R} stands for \bold{R}epeated running
    \code{\link{median}}s of \bold{3}.
}
\value{
    A vector or time series containing the smoothed values.
    Note that there are other smoothing methods which provide
    rather better results.
}
\references{Tukey (1977). \emph{E.D.A}; see ref.\ in \code{\link{line}}.}
\seealso{\code{\link{lowess}}.}
\examples{
library(eda)
data(presidents)
presidents[is.na(presidents)] <- 0
plot(presidents)
lines(smooth(presidents))
}
\keyword{robust}
\keyword{smooth}
