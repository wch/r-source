\name{pressure}
\alias{pressure}
\title{Vapor Pressure of Mercury as a Function of Temperature}
\usage{data(pressure)}
\description{
    Data on the relation between temperature in degrees Celsius and
    vapor pressure of mercury in millimeters (of mercury).
}
\format{
    A data frame with 19 observations on 2 variables.
    \tabular{rlll}{
    	[, 1] \tab temperature \tab numeric \tab temperature (deg C) \cr
	[, 2] \tab pressure    \tab numeric \tab pressure (mm)
    }
}
\source{
    R. C. Weast, Editor (1973).
    \emph{Handbook of Chemistry and Physics}.
    CRC Press.
}
\references{
    McNeil, D. (1977).
    \emph{Interactive Data Analysis}.
    New York: Wiley.
}
\examples{
data(pressure)
plot(pressure, xlab = "Temperature (deg C)",
     ylab = "Pressure (mm of Hg)",
     main = "pressure data: Vapor Pressure of Mercury")
plot(pressure, xlab = "Temperature (deg C)",  log = "y",
     ylab = "Pressure (mm of Hg)",
     main = "pressure data: Vapor Pressure of Mercury")
}
\keyword{datasets}
