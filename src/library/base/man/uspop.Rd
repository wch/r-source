\name{uspop}
\title{The Population of the United States}
\usage{data(uspop)}
\alias{uspop}
\format{A time series of 19 values.}
\source{
    McNeil, D. R. (1977).  Interactive Data Analysis.  New York: Wiley.
}
\description{
    This data set gives the population of the United States (in millions)
    as recorded by the decennial census for the period 1790--1970.
}
\references{}
\examples{
data(uspop)
plot(uspop, log = "y", main = "uspop data", xlab = "Year",
     ylab = "U.S. Population (millions)")
}
\keyword{datasets}
