\name{MethodSupport}
\alias{setGeneric}
\alias{evalSelectedMethod}
\alias{OldEvalSelectedMethod}
\alias{getMethodsForDispatch}
\alias{hasMethod}
\alias{resetGeneric}
\title{Additional (Support) Functions for Methods}
\usage{
setGeneric(name, def, group=NULL, valueClass=NULL, where=1)

evalSelectedMethod(f, ev, fname)

getMethodsForDispatch(f)

hasMethod(f, signature=character())

resetGeneric(f)

}
\description{
  
\describe{
\item{\code{setGeneric}:}{ 
  Define \code{name} to be a generic  function, for which methods will be defined.  

  If there is already a non-generic function of this name, it will be used
to define the generic unless \code{def} is supplied, and the current function will
become the default method for the generic.

If \code{def} is supplied, this defines the generic function.  The default method for
a new generic will usually be an existing non-generic.  See the .Rd page

  }
\item{\code{evalSelectedMethod}:}{ 
  Evaluate the selected method, f, in the environment, ev.

The general case is that f is a closure (an ordinary function), in which case
the body is evaluated in the given environment.

The special cases (primitive and internal) have to be faked by going back to the
environment of the call to the generic.

  }
\item{\code{hasMethod}:}{ 
  returns \code{TRUE} if \code{f} is the name of a generic function with an (explicit) method for
this signature.

  
  }
\item{\code{resetGeneric}:}{ 
  reset the currently defined methods for this generic by un-caching any inherited
methods.

  You \emph{must} call this function when you change relevant inheritance information during a
session, to guarantee that the new information is used if this generic has already been
called.
}
  
}
}
\keyword{programming}
