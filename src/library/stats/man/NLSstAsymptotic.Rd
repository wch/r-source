% File src/library/stats/man/NLSstAsymptotic.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2010 R Core Team
% Distributed under GPL 2 or later

\name{NLSstAsymptotic}
\encoding{UTF-8}
\alias{NLSstAsymptotic}
\alias{NLSstAsymptotic.sortedXyData}
\title{Fit the Asymptotic Regression Model}
\usage{
NLSstAsymptotic(xy)
}
\arguments{
 \item{xy}{a \code{sortedXyData} object}
}
\description{
  Fits the asymptotic regression model, in the form \code{b0 +
      b1*(1-exp(-exp(lrc) * x)} to the \code{xy} data.
  This can be used as a building block in determining starting estimates
  for more complicated models.
}
\value{
  A numeric value of length 3 with components labelled \code{b0},
  \code{b1}, and \code{lrc}.  \code{b0} is the estimated intercept on
  the \code{y}-axis, \code{b1} is the estimated difference between the
  asymptote and the \code{y}-intercept, and \code{lrc} is the estimated
  logarithm of the rate constant.
}
\author{\enc{José}{Jose} Pinheiro and Douglas Bates}
\seealso{\code{\link{SSasymp}}}
\examples{
Lob.329 <- Loblolly[ Loblolly$Seed == "329", ]
print(NLSstAsymptotic(sortedXyData(expression(age),
                                   expression(height),
                                   Lob.329)), digits = 3)
}
\keyword{manip}
