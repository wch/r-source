\name{longley}
\title{Longley's Regression Data}
\usage{data(longley)}
\alias{longley}
\source{J. W. Longley (1967).
An appraisal of least-squares programs from the point of view of the user.
Journal of the American Statistical Association 62, 819-841.}
\format{A data.frame with 7 economical variables, observed yearly
  from 1947 to 1962 (n=16).
  \tabular{rl}{
    GNP.deflator:  \tab GNP implicit price  deflator (1954=100)\cr
    GNP         :  \tab Gross National Producr.\cr
    Unemployed  :  \tab number of unemployed \cr
    Armed.Forces:  \tab number of .. in the armed forces\cr
    Population  :  \tab `noninstitutionalized' population\cr
    \tab >= 14 years of age.\cr
    Year        :  \tab the year (time).\cr
    Employed    :  \tab number of people employed.\cr
  }
  The regression  lm(Employed ~ .)  is known to be highly collinear.
}
\examples{
# give the data set in the form it is used in S-plus:
data(longley)
longley.x <- data.matrix(longley[,1:6])
longley.y <- longley[,"Employed"]
}
\keyword{datasets}
