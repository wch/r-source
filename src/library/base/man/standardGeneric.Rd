\name{standardGeneric}
\alias{standardGeneric}
\alias{topicName}
\title{Formal Method System Placeholders}
\description{
  These routines are primitives used with the \code{methods} package.
  They should not be used without it and do not need to be called
  directly in any case.

  \describe{
    \item{\code{standardGeneric}:}{
      dispatch the method defined for generic function named \code{f},
      using the actual arguments in the frame from which
      \code{standardGeneric} is called.
    }
    \item{\code{topicName}:}{
      the string used internally to find documenation of the given type
      and topic.  Called by the \code{?} operator and the special prompt
      functions in the methods package.
    }
  }
}
\usage{
standardGeneric(f)
topicName(type, topic)
}
\author{ John Chambers }
\keyword{programming}
\keyword{internal}
