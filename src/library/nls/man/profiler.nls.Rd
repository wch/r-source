% $Id: profiler.nls.Rd,v 1.3 2000/01/17 16:23:10 ripley Exp $
\name{profiler.nls}
\alias{profiler.nls}
\title{ Constructor for Profiler Objects from nls Objects}
\usage{
profiler.nls(fitted)
}
\arguments{
 \item{fitted}{ the original fitted model object of class \code{\link{nls}}.}
}
\description{
    Create a profiler object for the model object \code{fitted} of class
    \code{\link{nls}}.
}
\value{
    An object of class \code{profiler.nls} which is a list with function
    elements
    \item{getFittedModel()}{
	the \code{\link{nlsModel}} object corresponding to \code{fitted}
    }
    \item{getFittedPars()}{
	See documentation for \code{\link{profiler}}
    }
    \item{setDefault(varying, params)}{
	See documentation for \code{\link{profiler}}
    }
    \item{getProfile(varying, params)}{
	In the returned list, \code{fstat} is the ratio of change in
	sum-of-squares and the residual standard error.

	For other details, see documentation for \code{\link{profiler}}
    }
}
\section{WARNING}{
    When using setDefault and getProfile together, the internal state of
    the fitted model may get changed. So after completing the profiling
    for a parameter, the internal states should be restored by a call to
    setDefault without any arguments. For example see below or the source
    for \code{\link{profile.nls}}.
}
\references{Bates, D.M. and Watts, D.G. (1988), \emph{Nonlinear Regression Analysis
    and Its Applications}, Wiley}
\author{Douglas M. Bates and Saikat DebRoy}
\seealso{\code{\link{nls}}, \code{\link{nlsModel}},
    \code{\link{profiler}}, \code{\link{profile.nls}}}
\examples{
library( nls )
data( BOD )
# obtain the fitted object
fm1 <- nls(demand ~ SSasympOrig( Time, A, lrc ), data = BOD)
# get the profile for the fitted model
prof1 <- profiler( fm1 )
# profile with A fixed at 16.0
prof1$getProfile(c(F,T), 16.0)
# vary lrc
prof1$setDefault(varying = c(F, T))
# fix A at 14.0 and starting estimate of lrc at -0.2
prof1$setDefault(params = c(14.0, -0.2))
# and get the profile
prof1$getProfile()
# finally, set defaults back to original estimates
prof1$setDefault()
}
\keyword{nonlinear}
\keyword{regression}
\keyword{models}
