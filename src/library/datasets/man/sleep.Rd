\name{sleep}
\docType{data}
\encoding{latin1}
\alias{sleep}
\title{Student's Sleep Data}
\description{
  Data which show the effect of two soporific drugs (increase in hours
  of sleep) on groups consisting of 10 patients each.
}
\usage{sleep}
\format{
  A data frame with 20 observations on 2 variables.
  \tabular{rlll}{
    [, 1] \tab extra \tab numeric \tab increase in hours of sleep\cr
    [, 2] \tab group \tab factor  \tab patient group
  }
}
\source{
  Student (1908)
  The probable error of the mean.
  \emph{Biometrika}, \bold{6}, 20.
}
\references{
  Scheffé, Henry (1959)
  \emph{The Analysis of Variance}.
  New York, NY: Wiley.
}
\examples{
require(stats)
## ANOVA
anova(lm(extra ~ group, data = sleep))
}
\keyword{datasets}
