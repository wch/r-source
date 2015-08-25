% File src/library/datasets/man/women.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{women}
\docType{data}
\alias{women}
\title{Average Heights and Weights for American Women}
\description{
  This data set gives the average heights and weights for American women
  aged 30--39.
}
\usage{women}
\format{A data frame with 15 observations on 2 variables.
  \tabular{rlll}{
    \code{[,1]}  \tab \code{height}  \tab numeric  \tab Height (in)\cr
    \code{[,2]}  \tab \code{weight}  \tab numeric  \tab Weight (lbs)
  }
}
\source{The World Almanac and Book of Facts, 1975.}
\details{
  The data set appears to have been taken from the American Society of
  Actuaries \emph{Build and Blood Pressure Study} for some (unknown to
  us) earlier year.

  The World Almanac notes: \dQuote{The figures represent weights in
    ordinary indoor clothing and shoes, and heights with shoes}.
}
\references{
  McNeil, D. R. (1977)
  \emph{Interactive Data Analysis}.
  Wiley.
}
\examples{
require(graphics)
plot(women, xlab = "Height (in)", ylab = "Weight (lb)",
     main = "women data: American women aged 30-39")
}
\keyword{datasets}
