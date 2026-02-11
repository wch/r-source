% File src/library/datasets/man/LifeCycleSavings.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2025 R Core Team
% Distributed under GPL 2 or later

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
    [,1]  \tab \code{sr}    \tab numeric  \tab aggregate personal savings \cr
    [,2]  \tab \code{pop15} \tab numeric  \tab \% of population under 15 \cr
    [,3]  \tab \code{pop75} \tab numeric  \tab \% of population over 75 \cr
    [,4]  \tab \code{dpi}   \tab numeric  \tab real per-capita disposable
    income \cr
    [,5]  \tab \code{ddpi}  \tab numeric  \tab \% growth rate of dpi
  }
}
\source{
  The data were obtained from
  \bibcitet{|R:Belsley+Kuh+Welsch:1980|pp.\sspace{}41--42}.
  They in turn obtained the data from \bibcitet{R:Sterling:1977}.
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
\note{
  The dataset in \R < 4.6.0 had a typo in the row name \code{"Guatemala"}.
  The examples show how to recreate that version.
}
\references{
  \bibshow{*}
}
\examples{
require(stats); require(graphics)
pairs(LifeCycleSavings, panel = panel.smooth,
      main = "LifeCycleSavings data")
fm1 <- lm(sr ~ pop15 + pop75 + dpi + ddpi, data = LifeCycleSavings)
summary(fm1)

## recreate pre-4.6 dataset with a misspelled row name
idx <- which(row.names(LifeCycleSavings) == "Guatemala")
stopifnot(length(idx) == 1)
LCS <- LifeCycleSavings; row.names(LCS)[idx] <- "Guatamala" # sic!
}
\keyword{datasets}
