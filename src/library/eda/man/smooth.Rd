\name{smooth}
\alias{smooth}
\title{Median Smoothing}
\usage{
smooth(x)
}
\arguments{
  \item{x}{a vector or time series}
}
\description{
  This function applies Tukey's smoothing method \emph{3RSR}
  to \code{x}.
}
\value{
  A vector or time series containing the smoothed values.
  Note that there are other smoothing methods which provide
  rather better results. This one was designed for hand calculations.
}
\note{
  \emph{3R} stands for \bold{R}epeated running \code{\link{median}}s
  of \bold{3}.
}
\references{
  Tukey, J. W. (1977).
  \emph{Exploratory Data Analysis},
  Reading Massachusetts: Addison-Wesley.
}
\seealso{\code{\link{lowess}}; \code{\link[modreg]{loess}},
  \code{\link[modreg]{supsmu}} and
  \code{\link[modreg]{smooth.spline}} in package \file{modreg}.}
\examples{
library(eda)
data(presidents)
presidents[is.na(presidents)] <- 0
plot(presidents)
lines(smooth(presidents))
}
\keyword{robust}
\keyword{smooth}
