% File src/library/datasets/man/USJudgeRatings.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{USJudgeRatings}
\docType{data}
\alias{USJudgeRatings}
\title{Lawyers' Ratings of State Judges in the US Superior Court}
\description{
  Lawyers' ratings of state judges in the US Superior Court.
}
\usage{USJudgeRatings}
\format{
  A data frame containing 43 observations on 12 numeric variables.
  \tabular{rll}{
    [,1] \tab \code{CONT} \tab Number of contacts of lawyer with judge.\cr
    [,2] \tab \code{INTG} \tab Judicial integrity.\cr
    [,3] \tab \code{DMNR} \tab Demeanor.\cr
    [,4] \tab \code{DILG} \tab Diligence.\cr
    [,5] \tab \code{CFMG} \tab Case flow managing.\cr
    [,6] \tab \code{DECI} \tab Prompt decisions.\cr
    [,7] \tab \code{PREP} \tab Preparation for trial.\cr
    [,8] \tab \code{FAMI} \tab Familiarity with law.\cr
    [,9] \tab \code{ORAL} \tab Sound oral rulings.\cr
    [,10] \tab \code{WRIT} \tab Sound written rulings.\cr
    [,11] \tab \code{PHYS} \tab Physical ability.\cr
    [,12] \tab \code{RTEN} \tab Worthy of retention.
  }
}
\source{
  New Haven Register, 14 January, 1977 (from John Hartigan).
}
\examples{
require(graphics)
pairs(USJudgeRatings, main = "USJudgeRatings data")
}
\keyword{datasets}
