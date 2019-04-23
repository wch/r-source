% File src/library/stats/man/profile.nls.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2019 R Core Team
% Distributed under GPL 2 or later

\name{profile.nls}
\alias{profile.nls}
\title{Method for Profiling nls Objects}
\description{
  Investigates the profile log-likelihood function for a fitted model of
  class \code{"nls"}.
}
\usage{
\method{profile}{nls}(fitted, which = 1:npar, maxpts = 100, alphamax = 0.01,
        delta.t = cutoff/5, \dots)
}
\arguments{
  \item{fitted}{the original fitted model object.}
  \item{which}{the original model parameters which should be profiled.
    This can be a numeric or character vector.
    By default, all non-linear parameters are profiled.}
  \item{maxpts}{maximum number of points to be used for profiling each
    parameter.}
  \item{alphamax}{highest significance level allowed
    for the profile t-statistics.}
  \item{delta.t}{suggested change on the scale of the profile
    t-statistics.  Default value chosen to allow profiling at about
    10 parameter values.}
  \item{\dots}{further arguments passed to or from other methods.}
}
\value{
  A list with an element for each parameter being profiled. The elements
  are data-frames with two variables
  \item{par.vals}{a matrix of parameter values for each fitted model.}
  \item{tau}{the profile t-statistics.}
}
\details{
  The profile t-statistics is defined as the square root of change in
  sum-of-squares divided by residual standard error with an
  appropriate sign.
}
\references{
  Bates, D. M. and Watts, D. G. (1988), \emph{Nonlinear Regression Analysis
    and Its Applications}, Wiley (chapter 6).
}
\author{
  Of the original version,
  Douglas M. Bates and Saikat DebRoy
}
\seealso{
  \code{\link{nls}}, \code{\link{profile}}, \code{\link{plot.profile.nls}}
}
\examples{
\dontshow{od <- options(digits = 4)}
# obtain the fitted object
fm1 <- nls(demand ~ SSasympOrig(Time, A, lrc), data = BOD)
# get the profile for the fitted model: default level is too extreme
pr1 <- profile(fm1, alpha = 0.05)
# profiled values for the two parameters
## IGNORE_RDIFF_BEGIN
pr1$A
pr1$lrc
## IGNORE_RDIFF_END
# see also example(plot.profile.nls)
\dontshow{options(od)}
}
\keyword{nonlinear}
\keyword{regression}
\keyword{models}
