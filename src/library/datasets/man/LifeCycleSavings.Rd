\name{LifeCycleSavings}
\docType{data}
\alias{LifeCycleSavings}
\title{Intercountry Life-Cycle Savings Data}
\description{
  Data on the savings ratio 1960--1970.
}
\usage{LifeCycleSavings}
\format{
  A data frame with 50 observations on 5 variables.
  \tabular{rlll}{
    [,1]  \tab sr    \tab numeric  \tab aggregate personal savings \cr
    [,2]  \tab pop15 \tab numeric  \tab \% of population under 15 \cr
    [,3]  \tab pop75 \tab numeric  \tab \% of population over 75 \cr
    [,4]  \tab dpi   \tab numeric  \tab real per-capita disposable
    income \cr
    [,5]  \tab ddpi  \tab numeric  \tab \% growth rate of dpi
  }
}
\source{
  The data were obtained from Belsley, Kuh and Welsch (1980).
  They in turn obtained the data from Sterling (1977).
}
\details{
  Under the life-cycle savings hypothesis as developed by Franco
  Modigliani, the savings ratio (aggregate personal saving divided by
  disposable income) is explained by per-capita disposable income, the
  percentage rate of change in per-capita disposable income, and two
  demographic variables: the percentage of population less than 15
  years old and the percentage of the population over 75 years old.
  The data are averaged over the decade 1960--1970 to remove the
  business cycle or other short-term fluctuations. 
}
\references{
  Sterling, Arnie (1977) Unpublished BS Thesis.
  Massachusetts Institute of Technology.

  Belsley, D. A., Kuh. E. and Welsch, R. E. (1980)
  \emph{Regression Diagnostics}.
  New York: Wiley.
}
\examples{
require(stats); require(graphics)
pairs(LifeCycleSavings, panel = panel.smooth,
      main = "LifeCycleSavings data")
fm1 <- lm(sr ~ pop15 + pop75 + dpi + ddpi, data = LifeCycleSavings)
summary(fm1)
}
\keyword{datasets}
