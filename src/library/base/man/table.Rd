\name{table}
\title{Cross Tabulation}
\usage{
table(x, \dots, exclude = c(NA, NaN))
}
\alias{table}
\arguments{
  \item{x}{an object which can be interpreted as a factor (including
    character strings) or a list
    whose components can be so interpreted}
  \item{\dots}{further objects to be used as factors}
  \item{exclude}{values to use in the exclude argument of \code{factor}
    when interpreting non-factor objects}
  }
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
