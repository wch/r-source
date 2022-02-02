% File src/library/datasets/man/sleep.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2012 R Core Team
% Copyright 1999-2012 The R Foundation
% Distributed under GPL 2 or later

\name{sleep}
\docType{data}
\encoding{UTF-8}
\alias{sleep}
\title{Student's Sleep Data}
\description{
  Data which show the effect of two soporific drugs (increase in hours
  of sleep compared to control) on 10 patients.
}
\usage{sleep}
\format{
  A data frame with 20 observations on 3 variables.
  \tabular{rlll}{
    [, 1] \tab extra \tab numeric \tab increase in hours of sleep\cr
    [, 2] \tab group \tab factor  \tab drug given\cr
    [, 3] \tab ID    \tab factor  \tab patient ID
  }
}
\source{
  Cushny, A. R. and Peebles, A. R. (1905)
  The action of optical isomers: II hyoscines.
  \emph{The Journal of Physiology} \bold{32}, 501--510.

  Student (1908)
  The probable error of the mean.
  \emph{Biometrika}, \bold{6}, 20.
}
\details{
  The \code{group} variable name may be misleading about the data:
  They represent measurements on 10 persons, not in groups.
  %% But we keep it (as [,2]) for back compatibility.
}
\references{
  \enc{Scheffé}{Scheffe}, Henry (1959)
  \emph{The Analysis of Variance}.
  New York, NY: Wiley.
}
\examples{
require(stats)
## Student's paired t-test
with(sleep,
     t.test(extra[group == 1],
            extra[group == 2], paired = TRUE))

## The sleep *prolongations*
sleep1 <- with(sleep, extra[group == 2] - extra[group == 1])
summary(sleep1)
stripchart(sleep1, method = "stack", xlab = "hours",
           main = "Sleep prolongation (n = 10)")
boxplot(sleep1, horizontal = TRUE, add = TRUE,
        at = .6, pars = list(boxwex = 0.5, staplewex = 0.25))
}
\keyword{datasets}
