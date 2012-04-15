% File src/library/datasets/man/nhtemp.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{nhtemp}
\docType{data}
\alias{nhtemp}
\title{Average Yearly Temperatures in New Haven}
\description{
  The mean annual temperature in degrees Fahrenheit in New Haven,
  Connecticut, from 1912 to 1971.
}
\usage{nhtemp}
\format{A time series of 60 observations.}
\source{
  Vaux, J. E. and Brinker, N. B. (1972)
  \emph{Cycles}, \bold{1972}, 117--121.
}
\references{
  McNeil, D. R. (1977)
  \emph{Interactive Data Analysis}.
  New York: Wiley.
}
\examples{
require(stats); require(graphics)
plot(nhtemp, main = "nhtemp data",
  ylab = "Mean annual temperature in New Haven, CT (deg. F)")
}
\keyword{datasets}
