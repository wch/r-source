\name{nhtemp}
\title{Average Yearly Temperatures in New Haven}
\usage{data(nhtemp)}
\alias{nhtemp}
\format{A time-series of 60 observations.
}
\source{Vaux, J. E. and N. B. Brinker (1972).  Cycles, 1972, 117-121.
}
\description{The mean annual temperature in degrees Fahrenheit in New Haven,
Connecticut, from 1912 to 1971.
}
\references{McNeil, D. R. (1977).  Interactive Data Analysis.  New York: Wiley.
}
\examples{
data(nhtemp)
plot(nhtemp, main = "nhtemp data",
  ylab = "Mean annual temperature in New Haven, CT (deg. F)")
}
\keyword{datasets}
