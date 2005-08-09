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
\title{MethodsList Objects}
\description{
  These functions create and manipulate \code{MethodsList} objects, the
  objects used in R to store methods for dispatch.  You should not call
  any of these functions from code that you want to port to S-Plus.
  Instead, use the functions described in the references.
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

listFromMlist(mlist, prefix = list())

linearizeMlist(mlist, inherited = TRUE)

finalDefaultMethod(mlist, fname = "NULL")

mergeMethods(m1, m2, genericLabel)

loadMethod(method, fname, envir)

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
      signatures and the corresonding definitions will be printed;
      otherwise, only the signatures.
    }

    \item{\code{listFromMlist}:}{
      Undo the recursive nature of the methods list, making a list of
      function definitions, with the names of the list being the
      corresponding signatures
      \code{prefix} is the partial signature (a named list of classes)
      to be prepended to the signatures in this object.

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
      The true default method for the methods list object \code{mlist}
      (the method that matches class \code{"ANY"} for as many arguments
      as are used in methods matching for this generic function).  If
      \code{mlist} is null, returns the function called \code{fname}, or
      \code{NULL} if there is no such function.
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
  }
}
\details{
  Note that \code{MethodsList} objects represent methods only in the \R
  implementation.  You can use them to find or manipulate information about
  methods, but avoid doing so if you want your code to port to S-Plus.
}
\references{
  The R package \pkg{methods} implements, with a few exceptions, the
  programming interface for classes and methods in the book
  \emph{Programming with Data} (John M. Chambers, Springer, 1998), in
  particular sections 1.6, 2.7, 2.8, and chapters 7 and 8.

  While the programming interface for the \pkg{methods} package follows
  the reference, the R software is an original implementation, so
  details in the reference that reflect the S4 implementation may appear
  differently in R.  Also, there are extensions to the programming
  interface developed more recently than the reference.  For a
  discussion of details and ongoing development, see the web page
  \url{http://developer.r-project.org/methodsPackage.html} and the
  pointers from that page.
}
\keyword{internal}

