% File src/library/datasets/man/co2.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{co2}
\docType{data}
\alias{co2}
\title{\I{Mauna Loa} Atmospheric CO2 Concentration}
\description{
  Atmospheric concentrations of CO\eqn{_2}{2} are expressed in parts per
  million (ppm) and reported in the preliminary 1997 \abbr{SIO} manometric mole
  fraction scale.
}
\usage{co2}
\format{
  A time series of 468 observations; monthly from 1959 to 1997.
}
\details{
  The values for February, March and April of 1964 were missing and have
  been obtained by interpolating linearly between the values for January
  and May of 1964.
}
\source{
  Keeling, C. D. and  Whorf, T. P.,
  Scripps Institution of Oceanography (\abbr{SIO}),
  University of California,
  La Jolla, California USA 92093-0220.

  \url{https://scrippsco2.ucsd.edu/data/atmospheric_co2/}.

  Note that the data are subject to revision (based on recalibration of
  standard gases) by the Scripps institute, and hence may not agree
  exactly with the data provided by \R.
}
\references{
  Cleveland, W. S. (1993)
  \emph{Visualizing Data}.
  New Jersey: Summit Press.
}
\examples{
require(graphics)
plot(co2, ylab = expression("Atmospheric concentration of CO"[2]),
     las = 1)
title(main = "co2 data set")
}
\keyword{datasets}
