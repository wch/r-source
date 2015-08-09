% File src/library/datasets/man/volcano.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{volcano}
\docType{data}
\alias{volcano}
\title{Topographic Information on Auckland's Maunga Whau Volcano}
\description{
  Maunga Whau (Mt Eden) is one of about 50 volcanos in the Auckland
  volcanic field.  This data set gives topographic information for
  Maunga Whau on a 10m by 10m grid.
}
\usage{volcano}
\format{
  A matrix with 87 rows and 61 columns, rows corresponding to grid lines
  running east to west and columns to grid lines running south to
  north.}
\source{
  Digitized from a topographic map by Ross Ihaka.
  These data should not be regarded as accurate.
}
% FIXME: missing \references{}
\seealso{\code{\link{filled.contour}} for a nice plot.}
\examples{
require(grDevices); require(graphics)
filled.contour(volcano, color.palette = terrain.colors, asp = 1)
title(main = "volcano data: filled contour map")
}
\keyword{datasets}
