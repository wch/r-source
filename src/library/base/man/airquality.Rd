\name{airquality}
\title{New York Air Quality Measurements}
\usage{data(airquality)}
\alias{airquality}
\format{A data frame with 154 observations on 6 variables.
  \tabular{rlll}{
    [,1]  \tab Ozone    \tab numeric  \tab Ozone (ppb) \cr
    [,2]  \tab Solar.R  \tab numeric  \tab Solar R (lang) \cr
    [,3]  \tab Wind     \tab numeric  \tab Wind (mph) \cr
    [,4]  \tab Temp     \tab numeric  \tab Temperature (degrees F) \cr
    [,5]  \tab Month    \tab numeric  \tab Month 1--12 \cr
    [,6]  \tab Day      \tab numeric  \tab Day of month 1--31 \cr
    }
}
\description{
  Daily air quality measurements in New York. May to September 1973.
}
\details{
  Daily readings of the following air quality values for
  May 1, 1973 (a Tuesday) to September 30, 1973.
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
  Chambers, J. M., W. S. Cleveland, B. Kleiner and P. A. Tukey (1983).
  \emph{Graphical Methods for Data Analysis.}
  Belmont, CA: Wadsworth.
}
\examples{
data(airquality)
pairs(airquality, panel = panel.smooth, main = "airquality data")
}
\keyword{datasets}
