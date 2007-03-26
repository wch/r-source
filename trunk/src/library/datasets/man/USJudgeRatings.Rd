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
    [,1] \tab CONT \tab Number of contacts of lawyer with judge.\cr
    [,2] \tab INTG \tab Judicial integrity.\cr
    [,3] \tab DMNR \tab Demeanor.\cr
    [,4] \tab DILG \tab Diligence.\cr
    [,5] \tab CFMG \tab Case flow managing.\cr
    [,6] \tab DECI \tab Prompt decisions.\cr
    [,7] \tab PREP \tab Preparation for trial.\cr
    [,8] \tab FAMI \tab Familiarity with law.\cr
    [,9] \tab ORAL \tab Sound oral rulings.\cr
    [,10] \tab WRIT \tab Sound written rulings.\cr
    [,11] \tab PHYS \tab Physical ability.\cr
    [,12] \tab RTEN \tab Worthy of retention.
  }
}
\source{
  New Haven Register, 14 January, 1977 (from John Hartigan).
}
\examples{
pairs(USJudgeRatings, main = "USJudgeRatings data")
}
\keyword{datasets}
