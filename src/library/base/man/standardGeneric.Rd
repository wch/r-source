\name{standardGeneric}
\alias{standardGeneric}
\alias{objWithClass}
\alias{dataClass}
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
    \item{\code{objWithClass}:}{
      return the result of setting the class of \code{object} to
      \code{value}.  Defined as a separate primitive function because R
      types cannot generally be changed in place.
    }
    \item{\code{dataClass}:}{ returns a single string for the class of
      \code{object} even in the case that the object has an old-style
      class attribute with multiple strings.
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
objWithClass(object, value)
dataClass(object)
topicName(type, topic)
}
\author{ John Chambers }
\keyword{programming}
\keyword{internal}
