\name{MethodsList}
\alias{MethodsList}
\alias{makeMethodsList}
\alias{SignatureMethod}
\alias{insertMethod}
\alias{MethodsListDispatch}
\alias{matchArg}
\alias{matchArgClass}
\alias{showMlist}
\alias{print.MethodsList}
\alias{emptyMethodsList}
\alias{insertMethodInEmptyList}
\alias{listFromMlist}
\alias{listFromMlistForPrint}
\alias{finalDefaultMethod}
\alias{mergeAllMethods}
\alias{mergeMethods}
\title{MethodsList Objects}
\usage{
MethodsList(.ArgName, ...)

makeMethodsList(object, level=1)

SignatureMethod(names, signature, definition)

insertMethod(mlist, signature, args, def)

MethodsListDispatch(fname, ev, mustFind = TRUE)

matchArg(object, thisClass, mlist, ev)

matchArgClass(Class, classes, methods)

showMlist(mlist, includeDefs=TRUE, inherited = TRUE)

print.MethodsList(x, ...)

listFromMlist(mlist, prefix = list())

listFromMlistForPrint(mlist, inherited = TRUE, useArgName = TRUE, prev = character())

finalDefaultMethod(mlist, fname = "")

mergeAllMethods(mlist, update, thisClass, argument)

mergeMethods(m1, m2)

}
\description{

These functions create and manipulate \code{MethodsList} objects, the
objects used in R to store methods for dispatch.
You should not call any of these functions from code that you want to
port to S-Plus.  Instead, use the functions described in the
documentation.

\describe{
  
  \item{\code{MethodsList}:}{ 
  Create a MethodsList object out of the arguments.

Conceptually, this object is a named collection of methods to be dispatched
when the (first) argument in a function call matches the class corresponding to
one of the names.  A final, unnamed element (i.e., with name \code{""}) corresponds
to the default method.

The elements can be either a function, or another MethodsList.  In the second case,
this list implies dispatching on the second argument to the function using that
list, given a selection of this element on the first argument.  Thus, method dispatching
on an arbitrary number of arguments is defined.

MethodsList objects are used primarily to dispatch OOP-style methods and, in R,
to emulate S4-style methods.

  
  }
\item{\code{SignatureMethod}:}{ 
  construct a MethodsList object containing (only) this method, corresponding
to the signature; i.e., such that signature[[1]] is the match for the first
argument, signature[[2]] for the second argument, and so on.  The string
"missing" means a match for a missing argument, and "ANY" means use this as the
default setting at this level.

The first argument is the argument names to be used for dispatch corresponding to
the signatures.

  
  }
\item{\code{insertMethod}:}{ 
  insert the definition \code{def} into the MethodsList object, \code{mlist}, corresponding to
the signature

  
  }
\item{\code{MethodsListDispatch}:}{ 
  select the element of a MethodsList object corresponding to the
actual arguments.

Works recursively.  At each level finds an argument name from the current \code{mlist}
object, and evaluates this argument (if it is not missing), then uses the
\code{data.class} of the result to select an element of \code{mlist}.  If such an element
exists and is another \code{MethodsList} object, \code{MethodsListDispatch}  calls itself recursively
to resolve using further arguments.  Matching includes using a default selection or
a method specifically linked to class \code{"missing"}.  Once a function is found, it
is returned as the value.  If matching fails, an error occurs.

  
  }
\item{\code{matchArg, matchArgClass}:}{ 
  Utility functions to match the object  or the class to the elements of
  a methods list.
  Used in finding inherited methods, and not meant to be called directly.

  }
\item{\code{showMlist}:}{ 
  Prints the contents of the MethodsList.  If \code{includeDefs} the signatures and the
corresonding definitions will be printed; otherwise, only the signatures.

The function calls itself recursively.  \code{prev} is the previously
selected classes.
}
\item{\code{listFromMlistForPrint}:}{
  Undo the recursive nature of the methods list, making a list of function
  defintions, with the names of the list being the corresponding signatures
  (designed for printing; for looping over the methods, use \code{\link{listFromMlist}} instead).
  
  The function calls itself recursively:  \code{prev} is the previously
  selected classes.
  }

  \item{\code{finalDefaultMethod}}{
    The true default method for the methods list object \code{mlist}
    (the method that matches class \code{"ANY"} for as many arguments as
    are used in methods matching for this generic function).}

  \item{\code{mergeAllMethods}}{
    Merges the \code{AllMethods} slots in two methods list objects (including
    special  cases of NULL and function).
  }

  \item{\code{mergeMethods}}{
    Merges the methods in the second MethodsList object into the first,
    and returns the merged result.  Called from
    \code{\link{mergeGenericFunctions}}
    }
}

}
\details{
  Note that \code{MethodsList} objects represent methods only in the \R
  implementation.  You can use them to find or manipulate information about
  methods, but avoid doing so if you want your code to port to S-Plus.
}
\references{
The web page \url{http://www.omegahat.org/RSMethods/index.html} is the primary documentation.

The functions in this package emulate the facility for classes and methods described in
\emph{Programming with Data}, (John M. Chambers, Springer, 1998).  See this book
for further details and examples.
}
\author{
  John Chambers
}
\examples{
}
\keyword{internal}

