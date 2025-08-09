\name{gait}
\title{Hip and Knee Angle while Walking}
\docType{data}
\alias{gait}
\description{
  Hip and knee angle (in degrees) through a 20 point movement cycle for 39 boys.
}
\usage{gait}
\format{
  A 3-dimensional array with dimensions \code{c(20, 39, 2)} giving the
  \code{"Hip Angle"} and \code{"Knee Angle"} (in degrees) for 39 repetitions of 
  a 20 point gait cycle (over standardized gait times).
}
\details{
  The named components of \code{\link{dimnames}(gait)} are as follows:
  \describe{
     \item{\code{Time}}{\code{seq(from = 0.025, to = 0.975, by = 0.05)}}
     \item{\code{Subject}}{\code{"boy1"}, \code{"boy2"}, ..., \code{"boy39"}}
     \item{\code{Variable}}{\code{"Hip Angle"} and \code{"Knee Angle"}}
  }
}
\source{
  In the FDA book (2006), p.8, \emph{\dQuote{The Motion Analysis Laboratory at
  Children's Hospital, San Diego, collected these data}}.
}
\note{
  This is the version of the data as in the \CRANpkg{fda} package and 
  corresponding textbooks, but with named dimensions. One record appears to be 
  duplicated from the original paper,
  \bibcitet{R:Olshen+Biden+Wyatt:1989}, which 
  analyses a sample of 38 boys. The \code{gait} dataset has 39 boys but 
  \code{boy19} and \code{boy26} have identical measurements.
}
\references{
  \bibshow{*, R:Ramsay+Silverman:2005, R:Ramsay:2023}
}
\examples{
plot(gait[, 1, ], type = "b",
     xlim = range(gait[,,1]), ylim = range(gait[,,2]), 
     xlab = "Hip Angle", ylab = "Knee Angle", main = "'gait' data : Boy 1")
mtext("all other boys", col = "thistle"); grid()
matlines(gait[, -1, 1], gait[, -1, 2], type = "l", lty = 1, col = adjustcolor("thistle", 1/3))

## The data array, two matrices :
op <- options(width = 128) # on a wide console
aperm(gait, c(2:1, 3))
options(op)
}
\keyword{datasets}
