\name{Methods}
\alias{isGeneric}
\alias{removeGeneric}
\alias{standardGeneric}
\alias{dumpMethod}
\alias{existsFunction}
\alias{findFunction}
\alias{dumpMethods}
\alias{showMethods}
\alias{removeMethods}
\alias{signature}
\title{Defining Methods and Generic Functions}
\usage{
isGeneric(f, where=-1, fdef)

removeGeneric(name, where=-1)

standardGeneric(fname)

dumpMethod(generic, signature=character(), file=defaultDumpName(generic, signature), where=-1, def=getMethod(generic, signature, where = where, optional = T))

existsFunction(name, generic=TRUE)

findFunction(name, generic=TRUE)

dumpMethods(f, file="", signature=character(), methods, where=-1)

signature(...)

showMethods(f, where, classes, includeDefs=F)

removeMethods(f, where)

}
\description{

These are the functions that deal with formally defined methods.
Brief documentation is provided below.  See the references for an
introduction and for more details.
  
  \code{isGeneric}: 
  Is there a function named \code{f}, and if so, is it a generic?  
  
  \code{removeGeneric}: 
  Remove the generic function of this name.  
  
  \code{standardGeneric}: 
  Dispatches a method from the current function call for the generic function \code{fname}.  
  
  \code{getMethods}: 
  The list of methods for the specified generic.  
  
  \code{getMethod}: 
  Return the function that is defined as the method for this generic function and signature
(classes to be matched to the arguments of the generic function).

  
  \code{dumpMethod}: 
  Dump the method for this generic function and signature.  
  
  \code{existsFunction}: 
  Is there a function of this name. If \code{generic==FALSE}, generic functions are not counted.

  
  \code{findFunction}: 
  return all the elements of the search list on which a function
definition for \code{name} exists.  
  
  \code{selectMethod}: 
  Returns the method (a function) that R would use to evaluate a call to this generic,
with arguments corresponding to the specified signature.

f = the name of the generic function
sig = the signature of classes to match to the arguments of f.  
  
  \code{dumpMethods}: 
  Dump all the methods for this generic.  
  \code{signature}: 
  Returns a named list of classes to be matched to arguments of a
  generic function.
 
  \code{showMethods}: 
  Show all the methods for the specified function.

If \code{where} is supplied, the definition from that database will be used; otherwise,
the current definition is used (which will include inherited methods that have arisen so
far in the session).

The argument \code{classes} is included for consistency with S-Plus, but currently ignored.
Similarly, omitting \code{f} is not currently supported.  
  
  \code{removeMethods}: 
  removes all the methods defined for this generic function.  
}
\details{
  
  \code{setGeneric}: 
  

If there is already a non-generic function of this name, it will be used
to define the generic unless \code{def} is supplied, and the current function will
become the default method for the generic.

If \code{def} is supplied, this defines the generic function, and no default method
will exist (often a good feature, if the function should only be available for
a meaningful subset of all objects).

Arguments \code{group} and \code{valueClass} are retained for consistency with S-Plus, but
are currently not used.

  
  \code{isGeneric}: 
  

If the \code{fdef} argument is supplied, take this as the definition of the
generic, and test whether it is really a generic, with \code{f} as the name of
the generic.  (This argument is not available in S-Plus.)

  
  \code{removeGeneric}: 
  

If \code{where} supplied, just remove the version on this element of the search list;
otherwise, removes the first version encountered.

  
  \code{standardGeneric}: 
  
Generic functions should usually have a call to \code{standardGeneric} as their entire
body.  They can, however, do any other computations as well.

The usual \code{setGeneric} (directly or through calling
\code{setMethod}) creates a function with a call to
\code{standardGeneric}.

  
  \code{getMethods}: 
  If the function is not
a generic function, returns NULL.  
The \code{f} argument can be either the character string name of the generic
or the object itself

The \code{where} argument optionally says where to look for the function, if
\code{f} is given as the name.

  
  \code{dumpMethod}: 
   
The resulting source file will recreate the method.

  
  \code{findFunction}: 
  If \code{generic} is FALSE, ignore generic
functions.

  
  \code{selectMethod}: 
  The vector of strings
for the classes can be named or not.  If named, the names must match formal
argument names of f.  If not named, the signature is assumed to apply to the
arguments of f in order.
mustFind = If TRUE, an error results if there is no method (or no unique method)
corresponding to this signature.  Otherwise may return NULL or a MethodsList
object.

  
  \code{dumpMethods}: 
  

If \code{signature} is supplied only the methods matching this initial signature
are dumped.  (This feature is not found in S-Plus:  don't use it if you want
compatibility.)

  \code{signature}: 
  The advantage
of using \code{signature} is to provide a check on which arguments you meant, as well
as clearer documentation in your method specification.  In addition, \code{signature}
checks that each of the elements is a single character string.

  \code{showMethods}: 
The output style is different from S-Plus in that it does not show the database
from which the definition comes, but can optionally include the method definitions,
if \code{includeDefs == TRUE}.

  
  \code{removeMethods}: 
  Returns \code{TRUE} if
\code{f} was a generic function, \code{FALSE} (silently) otherwise.

If there is a default method, the function will be re-assigned as
a simple function with this definition; otherwise, it will be removed.  The
assignment or removal can be controlled by optional argument \code{where}, which
defaults to the first element of the search list having a function called \code{f}.

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
\seealso{\code{\link{setGeneric}}, \code{\link{setClass}}}
\keyword{programming}
\keyword{classes}
\keyword{methods}
