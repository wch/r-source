% File src/library/datasets/man/JohnsonJohnson.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{JohnsonJohnson}
\docType{data}
\alias{JohnsonJohnson}
\title{Quarterly Earnings per Johnson & Johnson Share}
\description{
  Quarterly earnings (dollars) per Johnson & Johnson share 1960--80.
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
\examples{\donttest{
require(stats); require(graphics)
JJ <- log10(JohnsonJohnson)
plot(JJ)
## This example gives a possible-non-convergence warning on some
## platforms, but does seem to converge on x86 Linux and Windows.
(fit <- StructTS(JJ, type = "BSM"))
tsdiag(fit)
sm <- tsSmooth(fit)
plot(cbind(JJ, sm[, 1], sm[, 3]-0.5), plot.type = "single",
     col = c("black", "green", "blue"))
abline(h = -0.5, col = "grey60")

monthplot(fit)
}}
\keyword{datasets}
