% File src/library/datasets/man/precip.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2025 R Core Team
% Distributed under GPL 2 or later

\name{precip}
\docType{data}
\alias{precip}
\title{Annual Precipitation in Selected US Cities}
\description{
  The yearly amount of precipitation (rainfall) in inches,
  averaged over the 30-year period 1941--1970, for each of
  70 United States (and Puerto Rico) cities.
}
\usage{precip}
\format{
  A named vector of length 70.
}
\source{
  Statistical Abstracts of the United States, 1975, p. 192.
}
\references{
  \bibshow{R:McNeil:1977}
}
\note{
  The dataset version before \R 4.6.0 had typos in the names
  \code{"Pittsburgh"} and \code{"Bismarck"}, and before \R 3.3.3 also in
  \code{"Cincinnati"}.  The examples show how to recreate these versions.
}
\examples{
require(stats); require(graphics)
dotchart(precip[order(precip)], main = "precip data")
title(sub = "Average annual precipitation (in.)")

## Recreate pre-4.6.0 version of the dataset with typos in 2 names
precip.2 <- setNames(precip, local({
    nms <- names(precip)
    nms[nms == "Pittsburgh"] <- "Pittsburg"
    nms[nms == "Bismarck"] <- "Bismark"
    nms
}))
## Recreate original, pre-3.3.3 version of the dataset with one more typo
precip.O <- setNames(precip.2,
    replace(names(precip.2), names(precip.2) == "Cincinnati", "Cincinati"))

stopifnot(
    all(precip == precip.2), all(precip == precip.O), # just name changes
    setequal(match(c("Bismarck", "Cincinnati", "Pittsburgh"), names(precip)),
             c(45, 46, 52)),
    identical(names(precip)[-c(45, 46, 52)], names(precip.O)[-c(45, 46, 52)])
)
}
\keyword{datasets}
