% File src/library/methods/man/MethodSupport.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2009 R Core Team
% Distributed under GPL 2 or later

\name{MethodSupport}
\title{Additional (Support) Functions for Methods}
\alias{getMethodsForDispatch}
\alias{cacheMethod}
\alias{resetGeneric}
\alias{listFromMethods}
\description{
  These are \emph{internal} support routines for computations on formal methods.
}
\usage{
listFromMethods(generic, where, table)

getMethodsForDispatch(fdef, inherited = FALSE)

cacheMethod(f, sig, def, args, fdef, inherited = FALSE)

resetGeneric(f, fdef, mlist, where, deflt)
}
\section{Summary of Functions}{
  \describe{
    \item{\code{listFromMethods}:}{
        A list object describing the methods for the function \code{generic}, supplied either as the function or the name of the function.   For user code, the function \code{\link{findMethods}} or \code{\link{findMethodSignatures}} is recommended instead,  returning a simple list of methods or a character matrix of the signatures.

 If \code{where} is supplied, this should be an environment or search list position from which a table of methods for the generic will be taken.  If \code{table} is supplied, this is itself assumed to be such a table.  If neither argument is supplied, the table is taken directly from the generic function (that is, the current set of methods defined for this generic).

      Returns an object of class \code{"LinearMethodsList"} (see
      \linkS4class{LinearMethodsList}) describing all the methods in the
      relevant table.
    }
    \item{\code{resetGeneric}:}{
      reset the currently defined methods for the generic function named
      \code{f}, found in environment \code{where} or explicitly supplied
      as an argument.  Other arguments are obsolete and ignored.

      Called for its side effect of resetting all inherited methods in
      the generic function's internal table.
      Normally not called directly, since changes to methods and the
      loading and detaching  of packages all generate a call automatically.
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
      Get the table of methods (an \code{\link{environment}} since R
      version 2.6.0) representing the methods for function \code{f}.

      For user code, the function \code{\link{findMethods}} or
      \code{\link{findMethodSignatures}} is recommended instead, returning
      a simple list of methods or a character matrix of the signatures.
    }
  }
}
\keyword{internal}
