\name{sleep}
\docType{data}
\encoding{latin1}
\alias{sleep}
\title{Student's Sleep Data}
\description{
  Data which show the effect of two soporific drugs (increase in hours
  of sleep compared to control) on 10 patients.
}
\usage{sleep}
\format{
  A data frame with 20 observations on 2 variables.
  \tabular{rlll}{
    [, 1] \tab extra \tab numeric \tab increase in hours of sleep\cr
    [, 2] \tab group \tab factor  \tab drug given
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
\references{
  \enc{Scheffé}{Scheffe}, Henry (1959)
  \emph{The Analysis of Variance}.
  New York, NY: Wiley.
}
\examples{
require(stats)
## Student's paired t-test
t.test(extra ~ group, data = sleep, paired = TRUE)
}
\keyword{datasets}
