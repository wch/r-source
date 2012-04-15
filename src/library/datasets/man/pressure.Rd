% File src/library/datasets/man/pressure.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{pressure}
\docType{data}
\alias{pressure}
\title{Vapor Pressure of Mercury as a Function of Temperature}
\description{
  Data on the relation between temperature in degrees Celsius and vapor
  pressure of mercury in millimeters (of mercury).
}
\usage{pressure}
\format{
  A data frame with 19 observations on 2 variables.
  \tabular{rlll}{
    [, 1] \tab temperature \tab numeric \tab temperature (deg C)\cr
    [, 2] \tab pressure    \tab numeric \tab pressure (mm)
  }
}
\source{
  Weast, R. C., ed. (1973)
  \emph{Handbook of Chemistry and Physics}.
  CRC Press.
}
\references{
  McNeil, D. R. (1977)
  \emph{Interactive Data Analysis}.
  New York: Wiley.
}
\examples{
require(graphics)
plot(pressure, xlab = "Temperature (deg C)",
     ylab = "Pressure (mm of Hg)",
     main = "pressure data: Vapor Pressure of Mercury")
plot(pressure, xlab = "Temperature (deg C)",  log = "y",
     ylab = "Pressure (mm of Hg)",
     main = "pressure data: Vapor Pressure of Mercury")
}
\keyword{datasets}
