% $Id: profile.Rd,v 1.1 1999/11/12 13:34:35 bates Exp $
\name{profile}
\alias{profile}
\title{ Generic function for profiling non-linear models}
\usage{
profile(fitted, which, maxpts, ...)
}
\arguments{
 \item{fitted}{ the original fitted model object.}
 \item{which}{ the original model parameters which should be
     profiled. By default, all parameters are profiled.}
 \item{maxpts}{ Maximum number of points to be used for profiling each
     parameter.}
 \item{\dots}{  Additional parameters. See documentation on individual
     methods.}
}
\description{
 Investigates behavior of objective function near the solution
 represented by \code{fitted}.

 See documentation on method functions for further details.
}
\value{
  A list with an element for each parameter being profiled. The elements
  are data-frames with two variables.
  \item{par.vals}{ a matrix of parameter values for each fitted model.}
  \item{tau}{ profile statistics as defined in method documentation.}
}
\author{Douglas M. Bates and Saikat DebRoy}
\seealso{\code{\link{profile.nls}}, \code{\link{profiler}}}
\examples{
# see documentation on individual methods
}
\keyword{ nonlinear, models }
