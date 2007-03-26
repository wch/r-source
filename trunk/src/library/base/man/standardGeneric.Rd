\name{standardGeneric}
\alias{standardGeneric}
\title{Formal Method System -- Creating Generic Functions}% and other Placeholders
\description{
  This help page is for Routines which are primitives used with the
  \pkg{methods} package.  They should not be used without it.
  % was "and do not need to be called directly in any case."
  % but standardGeneric() should be "called directly" , right?
}
\usage{
standardGeneric(f)
}
\details{
  \code{standardGeneric} dispatches the method defined for a generic
  function \code{f}, using the actual arguments in the frame from which
  it is called.

  For more, use the \pkg{methods} package, and see the documentation in
  \code{\link{GenericFunctions}}.
}
\author{ John Chambers }
\keyword{programming}
\keyword{internal}
