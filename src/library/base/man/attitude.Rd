\name{attitude}
\alias{attitude}
\title{Attitudes Toward Supervisors}
\usage{data(attitude)}
\description{Not available (yet).}
\format{A data from with 30 observations on 7 variables.
  \tabular{rlll}{
      [,1] \tab rating   \tab numeric  \tab Overall rating \cr
      [,2] \tab complaints\tab numeric \tab Handling of employee complaints \cr
      [,3] \tab privileges\tab numeric \tab Gives special privileges \cr
      [,4] \tab learning \tab numeric  \tab Opportunity to learn \cr      
      [,5] \tab raises   \tab numeric  \tab Gives raises \cr
      [,6] \tab critical \tab numeric  \tab Too critical \cr
      [,7] \tab advancel \tab numeric  \tab Advancement  \cr      
  }
}
\source{
    S. Chatterjee and B. Price (1977).
    \emph{Regression Analysis by Example}.
    New York: Wiley.
}
\examples{
data(attitude)
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
