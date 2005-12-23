\name{profiler}
\alias{profiler}
\title{ Constructor for Profiler Objects for Nonlinear Models}
\usage{
profiler(fitted, \dots)
}
\arguments{
 \item{fitted}{the original fitted model object.}
 \item{\dots}{Additional parameters.  See documentation on individual
   methods.}
}
\description{
  Create a profiler object for the model object \code{fitted}.
}
\value{
  An object of class \code{"profiler"} which is a list with function
  elements
  \item{getFittedPars()}{
    the parameters in \code{fitted}
  }
  \item{setDefault(varying, params)}{
    this is used for changing the default settings for
    profiling.  In absence of both parameters, the default is set
    to the original fitted parameters with all parameters
    varying.  The arguments are

    \code{varying}: a logical, integer or character vector giving
    parameters to be varied.
    \code{params}: the default value at which profiling is to take place.
  }
  \item{getProfile(varying, params)}{
    this can be used in conjunction with \code{setDefault}
    without any arguments. Alternatively, the parameters to be
    varied and the values for fixed parameters can be specified
    using the arguments. The arguments are

    \code{varying}: a logical vector giving parameters to be
    varied. This can be omitted if params is a named list or
    numeric vector.

    \code{params}: values for parameters to be held fixed.

    It returns a list with elements

    \code{parameters}: the parameter values for the profiled optimum.

    \code{fstat}: a profile statistics.  See individual methods for details.

    \code{varying}: a logical vector indicating parameters which were varied.
  }
}
\author{Douglas M. Bates and Saikat DebRoy}
\seealso{
  \code{\link{profiler.nls}}, \code{\link{profile}}
}
\keyword{nonlinear}
\keyword{models}
