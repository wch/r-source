\name{table}
\title{Cross Tabulation}
\usage{
table(\dots, exclude = c(NA, NaN))
}
\alias{table}
\arguments{
  \item{\dots}{objects which can be interpreted as factors (including
    character strings), or a list (or data frame) whose components can
    be so interpreted}
  \item{exclude}{values to use in the exclude argument of \code{factor}
    when interpreting non-factor objects}
  }
\description{
  \code{table} uses the cross-classifying factors to build a contingency
  table of the counts at each combination of factor levels.
}
\examples{
## Simple frequency distribution
table(rpois(100,5))
data(warpbreaks)
attach(warpbreaks)
## Check the design:
table(wool, tension)
data(state)
table(state.division, state.region)

data(airquality)
attach(airquality)
# simple two-way contingency table
table(cut(Temp, quantile(Temp)), Month)
}
\keyword{category}
