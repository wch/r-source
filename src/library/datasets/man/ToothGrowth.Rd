% File src/library/datasets/man/ToothGrowth.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2025 R Core Team
% Distributed under GPL 2 or later

\name{ToothGrowth}
\docType{data}
\alias{ToothGrowth}
\title{The Effect of Vitamin C on Tooth Growth in Guinea Pigs}
\description{
  The response is the length of odontoblasts (cells responsible for
  tooth growth) in 60 guinea pigs.  Each animal received one of three
  dose levels of vitamin C (0.5, 1, and 2 mg/day) by one of two delivery
  methods, orange juice or ascorbic acid (a form of vitamin C and coded
  as \code{VC}).
}
\usage{ToothGrowth}
\format{
  A data frame with 60 observations on 3 variables.
  \tabular{rlll}{
    [,1]  \tab \code{len}   \tab numeric  \tab Tooth length\cr
    [,2]  \tab \code{supp}  \tab factor   \tab Supplement type (\I{VC} or \I{OJ}).\cr
    [,3]  \tab \code{dose}  \tab numeric  \tab Dose in milligrams/day
  }
}
\source{
  \bibshow{R:Bliss:1952}
}
\references{
  \bibshow{R:Crampton:1947, R:McNeil:1977}
}
\examples{
require(graphics)
coplot(len ~ dose | supp, data = ToothGrowth, panel = panel.smooth,
       xlab = "ToothGrowth data: length vs dose, given type of supplement")
}
\keyword{datasets}
