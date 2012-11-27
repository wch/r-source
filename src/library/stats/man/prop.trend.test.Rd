% File src/library/stats/man/prop.trend.test.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2009 R Core Team
% Distributed under GPL 2 or later

\name{prop.trend.test}
\title{Test for trend in proportions}
\usage{
prop.trend.test(x, n, score = seq_along(x))
}
\alias{prop.trend.test}
\arguments{
  \item{x}{ Number of events }
  \item{n}{ Number of trials }
  \item{score}{ Group score }
}
\description{
  Performs chi-squared test for trend in proportions, i.e., a test
  asymptotically optimal for local alternatives where the log odds vary
  in proportion with \code{score}.  By default, \code{score} is chosen
  as the group numbers.
}
\value{
  An object of class \code{"htest"} with title, test statistic, p-value,
  etc.
}
\author{ Peter Dalgaard }
\note{ This really should get integrated with \code{prop.test} }
\seealso{ \code{\link{prop.test}}}
\examples{
smokers  <- c( 83, 90, 129, 70 )
patients <- c( 86, 93, 136, 82 )
prop.test(smokers, patients)
prop.trend.test(smokers, patients)
prop.trend.test(smokers, patients, c(0,0,0,1))
}
\keyword{htest}
