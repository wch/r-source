\name{MethodSupport}
\alias{.conflicts.OK}
\alias{OldEvalSelectedMethod}
\alias{getMethodsForDispatch}
\alias{cacheMethod}
\alias{resetGeneric}
\alias{listFromMethods}
\title{Additional (Support) Functions for Methods}
\description{
  These are support routines for computations on formal methods.
}
\usage{

listFromMethods(generic, where, table)

getMethodsForDispatch(f, fdef)

cacheMethod(f, sig, def, args, fdef, inherited = FALSE)

resetGeneric(f, fdef, mlist, where, deflt)

}
\section{Summary of Functions}{
  \describe{
    \item{\code{listFromMethods}:}{ 
        A list object describing the methods for the function \code{generic}, supplied either as the function or the name of the function.  If \code{where} is supplied, this should be an environment or search list position from which a table of methods for the generic will be taken.  If \code{table} is supplied, this is itself assumed to be such a table.  If neither argument is supplied, the table is taken directly from the generic function (that is, the current set of methods defined for this generic).

      Returns an object of class \code{"LinearMethodsList"} (see \link{LinearMethodsList-class}) describing all the methods in the relevant table.

      
    }
    \item{\code{resetGeneric}:}{ 
      reset the currently defined methods for this generic to the
      currently visible methods, looking from environment \code{where}.  Returns \code{TRUE}
      or \code{FALSE} according to whether information for the function
      was found in the metadata.

      Normally not called directly, since changes to methods,
    attaching and detaching packages all generate a call automatically.
    }
    \item{\code{cacheMethod}:}{
      Store the definition for this function and signature in the method
      metadata for the function.  Used to store extensions of coerce
      methods found through inheritance, and to cache methods with
      \code{\link{callNextMethod}} information.

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
