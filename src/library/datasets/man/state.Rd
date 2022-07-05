% File src/library/datasets/man/state.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2022 R Core Team
% Distributed under GPL 2 or later

\name{state}
\title{US State Facts and Figures}
\docType{data}
\encoding{UTF-8}
\alias{state}
\alias{state.abb}
\alias{state.area}
\alias{state.center}
\alias{state.division}
\alias{state.name}
\alias{state.region}
\alias{state.x77}
\description{
  Data sets related to the 50 states of the United States of America.
}
\usage{
state.abb
state.area
state.center
state.division
state.name
state.region
state.x77
}
\details{
  \R currently contains the following \dQuote{state} data sets.  Note
  that all data are arranged according to alphabetical order of the
  state names.
  \describe{
    \item{\code{state.abb}:}{character vector of 2-letter abbreviations
      for the state names.}
    \item{\code{state.area}:}{numeric vector of state areas (in square
      miles).}
    \item{\code{state.center}:}{ list with components named \code{x} and
      \code{y} giving the approximate geographic center of each state in
      negative longitude and latitude.  Alaska and Hawaii are placed
      just off the West Coast.  See \sQuote{Examples} on how to \dQuote{correct}.}
    \item{\code{state.division}:}{\code{\link{factor}} giving state divisions (New
      England, Middle Atlantic, South Atlantic, East South Central, West
      South Central, East North Central, West North Central, Mountain,
      and Pacific).}
    \item{\code{state.name}:}{character vector giving the full state
      names.}
    \item{\code{state.region}:}{\code{\link{factor}} giving the region (Northeast,
      South, North Central, West) that each state belongs to.}
    \item{\code{state.x77}:}{matrix with 50 rows and 8 columns giving
      the following statistics in the respective columns.
      \describe{
        \item{\code{Population}:}{population estimate as of July 1,
          1975}
        \item{\code{Income}:}{per capita income (1974)}
        \item{\code{Illiteracy}:}{illiteracy (1970, percent of
          population)}
        \item{\code{Life Exp}:}{life expectancy in years (1969--71)}
        \item{\code{Murder}:}{murder and non-negligent manslaughter rate
          per 100,000 population (1976)}
        \item{\code{HS Grad}:}{percent high-school graduates (1970)}
        \item{\code{Frost}:}{mean number of days with minimum
          temperature below freezing (1931--1960) in capital or large
          city}
       \item{\code{Area}:}{land area in square miles}
    }}
  }
  Note that a square mile is by definition exactly
  \code{(cm(1760 * 3 * 12) / 100 / 1000)^2} \eqn{km^2}, i.e.,
  \eqn{2.589988110336 km^2}.
}
\source{
  U.S. Department of Commerce, Bureau of the Census (1977)
  \emph{Statistical Abstract of the United States}.

  U.S. Department of Commerce, Bureau of the Census (1977)
  \emph{County and City Data Book}.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\examples{
(dst <- dxy <- data.frame(state.center, row.names=state.abb))
## Alaska and Hawaii are placed just off the West Coast (for compact map drawing):
dst[c("AK", "HI"),]
## state.center2 := version of state.center with "correct" coordinates for AK & HI:
## From https://pubs.usgs.gov/gip/Elevations-Distances/elvadist.html#Geographic\%20Centers
##   Alaska   63°50' N., 152°00' W., 60 miles northwest of Mount McKinley
##   Hawaii   20°15' N., 156°20' W., off Maui Island
dxy["AK",] <- c(-152.  , 63.83) # or  c(-152.11, 65.17)
dxy["HI",] <- c(-156.33, 20.25) # or  c(-156.69, 20.89)
state.center2 <- as.list(dxy)

plot(dxy, asp=1.2, pch=3, col=2)
text(state.center2, state.abb, cex=1/2, pos=4, offset=1/4)
i <- c("AK","HI")
do.call(arrows, c(setNames(c(dst[i,], dxy[i,]), c("x0","y0", "x1","y1")),
                  col=adjustcolor(4, .7), length=1/8))
points(dst[i,], col=2)
if(FALSE) { # if(require("maps")) \{
   map("state", interior = FALSE,          add = TRUE)
   map("state", boundary = FALSE, lty = 2, add = TRUE)
}
}% ex.
\keyword{datasets}
