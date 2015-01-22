% File src/library/methods/man/MethodsList.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{MethodsList}
\alias{MethodsList}
\alias{makeMethodsList}
\alias{SignatureMethod}
\alias{insertMethod}
\alias{inheritedSubMethodLists}
\alias{showMlist}
\alias{print.MethodsList}
\alias{emptyMethodsList}
\alias{insertMethodInEmptyList}
\alias{listFromMlist}
\alias{linearizeMlist}
\alias{finalDefaultMethod}
\alias{mergeMethods}
\alias{loadMethod}
\alias{loadMethod-methods}
\alias{loadMethod,ANY-method}
\alias{loadMethod,MethodDefinition-method}
\alias{loadMethod,MethodWithNext-method}
\alias{MethodsListSelect}
\title{MethodsList Objects}
\description{
  These functions create and manipulate \code{MethodsList} objects, the
  objects formerly used in R to store methods for dispatch.  Use of
  these objects is no longer required, and while not yet formally
  deprecated, will rarely be a good idea.  Where methods dispatch is
  to be studied, see \code{\link{selectMethod}}.  For computations
  that iterate over methods or over method signatures, see
  \code{\link{findMethods}}, which returns a linearized methods list
  to hold method definitions, usually more convenient for iteration
  than the recursive \code{MethodsList} objects.
}
\usage{
MethodsList(.ArgName, ...)

makeMethodsList(object, level=1)

SignatureMethod(names, signature, definition)

insertMethod(mlist, signature, args, def, cacheOnly)

inheritedSubMethodLists(object, thisClass, mlist, ev)

showMlist(mlist, includeDefs = TRUE, inherited = TRUE,
   classes, useArgNames, printTo = stdout() )

\method{print}{MethodsList}(x, ...)

listFromMlist(mlist, prefix = list(), sigs. = TRUE, methods. = TRUE)

linearizeMlist(mlist, inherited = TRUE)

finalDefaultMethod(method)

mergeMethods(m1, m2, genericLabel)

loadMethod(method, fname, envir)

MethodsListSelect(f, env, mlist, fEnv, finalDefault, evalArgs,
                  useInherited, fdef, resetAllowed)

}

\section{Details}{

  \describe{

    \item{\code{MethodsList}:}{
      Create a MethodsList object out of the arguments.

      Conceptually, this object is a named collection of methods to be
      dispatched when the (first) argument in a function call matches
      the class corresponding to one of the names.  A final, unnamed
      element (i.e., with name \code{""}) corresponds to the default
      method.

      The elements can be either a function, or another MethodsList.  In
      the second case, this list implies dispatching on the second
      argument to the function using that list, given a selection of
      this element on the first argument.  Thus, method dispatching on
      an arbitrary number of arguments is defined.

      MethodsList objects are used primarily to dispatch OOP-style
      methods and, in R, to emulate S4-style methods.
    }

    \item{\code{SignatureMethod}:}{
      construct a MethodsList object containing (only) this method,
      corresponding to the signature; i.e., such that
      \code{signature[[1]]} is the match for the first argument,
      \code{signature[[2]]} for the second argument, and so on.  The
      string \code{"missing"} means a match for a missing argument, and
      \code{"ANY"} means use this as the default setting at this level.

      The first argument is the argument names to be used for dispatch
      corresponding to the signatures.
    }

    \item{\code{insertMethod}:}{
      insert the definition \code{def} into the MethodsList object,
      \code{mlist}, corresponding to the signature.  By default, insert
      it in the slot \code{"methods"}, but \code{cacheOnly=TRUE} inserts
      it into the \code{"allMethods"} slot (used for dispatch but not saved).
    }

    \item{\code{inheritedSubMethodLists}:}{
      Utility function to match the object  or the class (if the object
      is \code{NULL}) to the elements of a methods list.
      Used in finding inherited methods, and not meant to be called
      directly.
    }

    \item{\code{showMlist}:}{
      Prints the contents of the MethodsList.  If \code{includeDefs} the
      signatures and the corresponding definitions will be printed;
      otherwise, only the signatures.
    }

    \item{\code{listFromMlist}:}{
      Undo the recursive nature of the methods list, making a list of
      \code{list(sigs,methods)} of function definitions, i.e.\sspace{}of
      matching signatures and methods.
      \code{prefix} is the partial signature (a named list of classes)
      to be prepended to the signatures in this object.  If \code{sigs.}
      or \code{methods.} are \code{FALSE}, the resulting part of the
      return value will be empty.

      A utility function used to iterate over all the individual methods
      in the object, it calls itself recursively.
    }

    \item{\code{linearizeMlist}:}{
      Undo the recursive nature of the methods list, making a list of
      function definitions, with the names of the list being the
      corresponding signatures.

      Designed for printing; for looping over the methods, use the above
      \code{listFromMlist} instead.
    }

    \item{\code{finalDefaultMethod}:}{
      The default method or NULL.  With the demise of
      \code{"MethodsList"} objects, this function only checks that the
      value given it is a method definition, primitive or NULL.
    }

    \item{\code{mergeMethods}:}{
      Merges the methods in the second MethodsList object into the
      first, and returns the merged result.  Called from
      \code{\link{getAllMethods}}.  For a primitive function,
      \code{genericLabel} is supplied as the name of the generic.
    }

    \item{\code{loadMethod}:}{
      Called, if necessary, just before a call to \code{method} is
      dispatched in the frame \code{envir}.  The function exists so that
      methods can be defined for special classes of objects.  Usually
      the point is to assign or modify information in the frame
      environment to be used evaluation.  For example, the standard
      class \code{MethodDefinition} has a method that stores the target
      and defined signatures in the environment.  Class
      \code{MethodWithNext} has a method taking account of the
      mechanism for storing the method to be used in a call to
      \code{\link{callNextMethod}}.

      Any methods defined for \code{loadMethod} must return the function
      definition to be used for this call; typically, this is just the
      \code{method} argument.
    }

  \item{\code{MethodsListSelect}}{
      The function \code{MethodsListSelect} performs a full search
      (including all inheritance and group generic information: see the
      \link{Methods} documentation page for details on how this works).
      The call returns a possibly revised methods list object,
      incorporating any method found as part of the \code{allMethods}
      slot.  This search was used by the evaluator when methods lists
      were the metadata for methods dispatch.  This function is now deprecated.

    }
  }
}
\note{
  Note that \code{MethodsList} objects represent methods only in the \R
  implementation.  You can use them to find or manipulate information about
  methods, but avoid doing so if you want your code to port to S-Plus.
}
\references{
 Chambers, John M. (2008)
 \emph{Software for Data Analysis: Programming with R}
  Springer.  (For the R version.)

 Chambers, John M. (1998)
 \emph{Programming with Data}
 Springer (For the original S4 version.)
}
\keyword{internal}

