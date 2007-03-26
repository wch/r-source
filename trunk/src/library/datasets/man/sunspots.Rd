\name{sunspots}
\docType{data}
\alias{sunspots}
\title{Monthly Sunspot Numbers, 1749--1983}
\description{
  Monthly mean relative sunspot numbers from 1749 to 1983.  Collected at
  Swiss Federal Observatory, Zurich until 1960, then Tokyo Astronomical
  Observatory.
}
\usage{sunspots}
\format{
  A time series of monthly data from 1749 to 1983.
}
\source{
  Andrews, D. F. and Herzberg, A. M. (1985)
  \emph{Data: A Collection of Problems from Many Fields for the
    Student and Research Worker}.
  New York: Springer-Verlag.
}
\seealso{
  \code{\link{sunspot.month}} has a longer (and a bit different) series.
} 
\examples{
plot(sunspots, main = "sunspots data", xlab = "Year",
     ylab = "Monthly sunspot numbers")
}
\keyword{datasets}
