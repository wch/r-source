\name{table}
\title{Cross Tabulation}
\usage{
table(\dots)
}
\alias{table}
\description{
\code{table} uses the cross-classifying factors provided as arguments
to build a contingency table of the counts at each combination
of factor levels.
}
\examples{
## Simple frequency distribution
table(rpois(100,5))
data(warpbreaks)
attach(warpbreaks)
## Check the design:
table(wool,tension)
data(state)
table(state.division, state.region)

data(airquality)
attach(airquality)
# simple two-way contingency table
table(cut(Temp, quantile(Temp)), Month)
}
\keyword{category}
