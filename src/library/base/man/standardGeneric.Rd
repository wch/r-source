\name{standardGeneric}
\alias{standardGeneric}
\title{Formal Method System Placeholders}
\description{
  Routines which are primitives used with the \pkg{methods} package.
  They should not be used without it and do not need to be called
  directly in any case.
}
\usage{
standardGeneric(f)
}
\details{
  \code{standardGeneric} dispatches the method defined for a generic
  function \code{f}, using the actual arguments in the frame from which
  it is called.
}
\author{ John Chambers }
\keyword{programming}
\keyword{internal}
