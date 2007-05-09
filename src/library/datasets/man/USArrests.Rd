\name{USArrests}
\docType{data}
\alias{USArrests}
\title{Violent Crime Rates by US State}
\description{
  This data set contains statistics, in arrests per 100,000 residents
  for assault, murder, and rape in each of the 50 US states in 1973.
  Also given is the percent of the population living in urban areas.
}
\usage{USArrests}
\format{
  A data frame with 50 observations on 4 variables.

  \tabular{rlll}{
    [,1]  \tab Murder    \tab numeric
    \tab Murder arrests (per 100,000)\cr
    [,2]  \tab Assault   \tab numeric
    \tab Assault arrests (per 100,000)\cr
    [,3]  \tab UrbanPop  \tab numeric
    \tab Percent urban population\cr
    [,4]  \tab Rape      \tab numeric
    \tab Rape arrests (per 100,000)
  }
}
\source{
  World Almanac and Book of facts 1975.  (Crime rates).

  Statistical Abstracts of the United States 1975.  (Urban rates).
}
\seealso{The \code{\link{state}} data sets.}
\references{
  McNeil, D. R. (1977)
  \emph{Interactive Data Analysis}.
  New York: Wiley.
}
\examples{
require(graphics)
pairs(USArrests, panel = panel.smooth, main = "USArrests data")
}
\keyword{datasets}
