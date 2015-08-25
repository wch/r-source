% File src/library/datasets/man/airquality.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{airquality}
\docType{data}
\alias{airquality}
\title{New York Air Quality Measurements}
\description{
  Daily air quality measurements in New York, May to September 1973.
}
\usage{airquality}
\format{
  A data frame with 154 observations on 6 variables.

  \tabular{rlll}{
    \code{[,1]} \tab \code{Ozone}   \tab numeric \tab Ozone (ppb)\cr
    \code{[,2]} \tab \code{Solar.R} \tab numeric \tab Solar R (lang)\cr
    \code{[,3]} \tab \code{Wind}    \tab numeric \tab Wind (mph)\cr
    \code{[,4]} \tab \code{Temp}    \tab numeric \tab Temperature (degrees F)\cr
    \code{[,5]} \tab \code{Month}   \tab numeric \tab Month (1--12)\cr
    \code{[,6]} \tab \code{Day}     \tab numeric \tab Day of month (1--31)
  }
}
\details{
  Daily readings of the following air quality values for May 1, 1973 (a
  Tuesday) to September 30, 1973.

  \itemize{
    \item \code{Ozone}: Mean ozone in parts per
    billion from 1300 to 1500 hours at Roosevelt Island
    \item \code{Solar.R}: Solar radiation
    in Langleys in the frequency band 4000--7700 Angstroms from
    0800 to 1200 hours at Central Park
    \item \code{Wind}: Average wind speed in miles
    per hour at 0700 and 1000 hours at LaGuardia Airport
    \item \code{Temp}: Maximum daily
    temperature in degrees Fahrenheit at La Guardia Airport.
  }
}
\source{
  The data were obtained from the New York State Department of
  Conservation (ozone data) and the National Weather Service
  (meteorological data).
}
\references{
  Chambers, J. M., Cleveland, W. S., Kleiner, B. and Tukey, P. A. (1983)
  \emph{Graphical Methods for Data Analysis}.
  Belmont, CA: Wadsworth.
}
\examples{
require(graphics)
pairs(airquality, panel = panel.smooth, main = "airquality data")
}
\keyword{datasets}
