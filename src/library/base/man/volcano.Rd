\name{volcano}
\title{Topographic Information for the Maunga Whau Volcano}
\usage{data(volcano)}
\alias{volcano}
\format{A matrix with 87 rows and 61 columns.
}
\source{Digitized from a topographic map by Ross Ihaka.
    These data should not be regarded as accurate.
}
\description{
    Maunga Whau (Mt Eden) is one of about 50 volcanos in the Auckland
    volcanic field.  This data set gives topographic information for
    Maunga Whau on a 10m by 10m grid.

    The data are in the form of a matrix with rows corresponding to
    grid lines running east to west and columns to grid lines running
    south to north.
}
\references{}
\seealso{\code{\link{filled.contour}} for a nice plot.}
\examples{
data(volcano)
filled.contour(volcano, color = terrain.colors, asp = 1)
title(main = "volcano data: filled contour map")
}
\keyword{datasets}
