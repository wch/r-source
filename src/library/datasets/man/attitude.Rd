\name{attitude}
\docType{data}
\alias{attitude}
\title{The Chatterjee--Price Attitude Data}
\description{
  From a survey of the clerical employees of a large financial
  organization, the data are aggregated from the questionnaires of the
  approximately 35 employees for each of 30 (randomly selected)
  departments.  The numbers give the percent proportion of favourable
  responses to seven questions in each department.}
\usage{attitude}
\format{
  A dataframe with 30 observations on 7 variables. The first column are
  the short names from the reference, the second one the variable names
  in the data frame:
  \tabular{rlll}{
     Y   \tab rating    \tab numeric  \tab Overall rating \cr
    X[1] \tab complaints\tab numeric \tab Handling of employee complaints \cr
    X[2] \tab privileges\tab numeric \tab Does not allow special privileges \cr
    X[3] \tab learning \tab numeric  \tab Opportunity to learn \cr
    X[4] \tab raises   \tab numeric  \tab Raises based on performance \cr
    X[5] \tab critical \tab numeric  \tab Too critical \cr
    X[6] \tab advancel \tab numeric  \tab Advancement}
}
\source{
  Chatterjee, S. and Price, B. (1977)
  \emph{Regression Analysis by Example}.
  New York: Wiley.
  (Section 3.7, p.68ff of 2nd ed.(1991).)
}
\examples{
require(stats)
pairs(attitude, main = "attitude data")
summary(attitude)
summary(fm1 <- lm(rating ~ ., data = attitude))
opar <- par(mfrow = c(2, 2), oma = c(0, 0, 1.1, 0),
            mar = c(4.1, 4.1, 2.1, 1.1))
plot(fm1)
summary(fm2 <- lm(rating ~ complaints, data = attitude))
plot(fm2)
par(opar)
}
\keyword{datasets}
