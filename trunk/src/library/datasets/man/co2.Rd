\name{co2}
\docType{data}
\alias{co2}
\title{Mauna Loa Atmospheric CO2 Concentration}
\description{
  Atmospheric concentrations of CO\eqn{_2}{2} are expressed in parts per
  million (ppm) and reported in the preliminary 1997 SIO manometric mole
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
  Scripps Institution of Oceanography (SIO),
  University of California,
  La Jolla, California USA 92093-0220.

  \url{ftp://cdiac.esd.ornl.gov/pub/maunaloa-co2/maunaloa.co2}.
}
\references{
  Cleveland, W. S. (1993)
  \emph{Visualizing Data}.
  New Jersey: Summit Press.
}
\examples{
plot(co2, ylab = expression("Atmospheric concentration of CO"[2]),
     las = 1)
title(main = "co2 data set")
}
\keyword{datasets}
