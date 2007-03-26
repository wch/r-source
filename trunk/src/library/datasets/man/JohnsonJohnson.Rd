\name{JohnsonJohnson}
\docType{data}
\alias{JohnsonJohnson}
\title{Quarterly Earnings per Johnson \& Johnson Share}
\description{
  Quarterly earnings (dollars) per Johnson \& Johnson share 1960--80.
}
\usage{JohnsonJohnson}
\format{
  A quarterly time series
}
\source{
  Shumway, R. H. and Stoffer, D. S. (2000)
  \emph{Time Series Analysis and its Applications}.
  Second Edition.  Springer.  Example 1.1.
}
\examples{
require(stats)
JJ <- log10(JohnsonJohnson)
plot(JJ)
(fit <- StructTS(JJ, type="BSM"))
tsdiag(fit)
sm <- tsSmooth(fit)
plot(cbind(JJ, sm[, 1], sm[, 3]-0.5), plot.type = "single",
     col = c("black", "green", "blue"))
abline(h = -0.5, col = "grey60")

monthplot(fit)
}
\keyword{datasets}
