\name{sunspots}
\alias{sunspots}
\title{Monthly Mean Relative Sunspot Numbers}
\usage{data(sunspots)}
\description{
    Monthly sunspot numbers from 1749 to 1983. Collected at Swiss
    Federal Observatory, Zurich until 1960, then Tokyo Astronomical
    Observatory.
}
\format{
    A time series of monthly data from 1749 to 1983.
}
\source{
    Andrews, D. F. and Herzberg, A. M. (1985)
    \emph{Data: A Collection of Problems from Many Fields for the
        Student and Research Worker}.
    New York: Springer-Verlag.
}
\examples{
data(sunspots)
plot(sunspots, main = "sunspots data", xlab = "Year",
     ylab = "Monthly sunspot numbers")
}
\keyword{datasets}
