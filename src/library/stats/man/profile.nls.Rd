\name{profile.nls}
\alias{profile.nls}
\title{Method for Profiling nls Objects}
\usage{
\method{profile}{nls}(fitted, which = 1:npar, maxpts = 100, alphamax = 0.01,
        delta.t = cutoff/5, \dots)
}
\arguments{
  \item{fitted}{ the original fitted model object.}
  \item{which}{ the original model parameters which should be
    profiled. By default, all parameters are profiled.}
  \item{maxpts}{ maximum number of points to be used for profiling each
    parameter.}
  \item{alphamax}{ maximum significance level allowed for the profile
    t-statistics.}
  \item{delta.t}{ suggested change on the scale of the profile
    t-statistics. Default value chosen to allow profiling at about
    10 parameter values.}
  \item{\dots}{further arguments passed to or from other methods.}
}
\description{
  Investigates behavior of the log-likelihood function near the solution
  represented by \code{fitted}.
}
\value{
  A list with an element for each parameter being profiled. The elements
  are data-frames with two variables 
  \item{par.vals}{ a matrix of parameter values for each fitted model.}
  \item{tau}{ The profile t-statistics.}
}
\details{
  Only fits with \code{algorithm = "default"} are supported.
  
  The profile t-statistics is defined as the square root of change in
  sum-of-squares divided by residual standard error with an
  appropriate sign.
}
\references{
  Bates, D.M. and Watts, D.G. (1988), \emph{Nonlinear Regression Analysis
	and Its Applications}, Wiley (chapter 6)}
\author{Douglas M. Bates and Saikat DebRoy}
\seealso{\code{\link{nls}}, \code{\link{profile}},
    \code{\link{profiler.nls}}, \code{\link{plot.profile.nls}}
}
\examples{
# obtain the fitted object
fm1 <- nls(demand ~ SSasympOrig( Time, A, lrc ), data = BOD)
# get the profile for the fitted model
pr1 <- profile( fm1 )
# profiled values for the two parameters
pr1$A
pr1$lrc
}
\keyword{nonlinear}
\keyword{regression}
\keyword{models}
