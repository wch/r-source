\name{MethodSupport}
\alias{.conflicts.OK}
\alias{OldEvalSelectedMethod}
\alias{getMethodsForDispatch}
\alias{cacheMethod}
\alias{hasMethod}
\alias{resetGeneric}
\title{Additional (Support) Functions for Methods}
\description{
  These are support routines for computations on formal methods.
}
\usage{
getMethodsForDispatch(f)

cacheMethod(f, sig, def, args = names(sig))

hasMethod(f, signature=character())

resetGeneric(f)
}
\section{Summary of Functions}{
  \describe{
    \item{\code{hasMethod}:}{ 
      returns \code{TRUE} if \code{f} is the name of a generic function
      with an (explicit) method for this signature.
    }
    \item{\code{resetGeneric}:}{ 
      reset the currently defined methods for this generic by removing
      the corresponding definition from the methods metadata (to be
      remerged when the function is next called).  Returns \code{TRUE}
      or \code{FALSE} according to whether information for the function
      was found in the metadata.

      You \emph{must} call this function when you change relevant
      inheritance information during a session, to guarantee that the
      new information is used if this generic has already been called.
    }
    \item{\code{cacheMethod}:}{
      Store the definition for this function and signature in the method
      metadata for the function.  Used to store extensions of coerce
      methods found through inheritance.

      No persistent effect, since the method metadata is session-scope
      only.
    }
    \item{\code{getMethodsForDispatch}:}{
      Get the current methods list object representing the methods for
      function \code{f}, merged from the various packages and with any
      additional caching information stored in the \code{allMethods}
      slot.

      If methods have not yet been merged, calling
      \code{getMethodsForDispatch} will cause the merge to take place.
    }
  }
}
\keyword{internal}
