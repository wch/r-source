\name{getMethod}
\alias{getMethod}
\alias{findMethod}
\alias{existsMethod}
\alias{getMethods}
\alias{selectMethod}
\alias{hasMethod}
\alias{MethodsListSelect}
\title{ Get or Test for the Definition of a Method }
\description{
  The functions \code{getMethod} and \code{selectMethod} get the
  definition of a particular method; the functions \code{existsMethod}
  and \code{hasMethod} test for the existence of a method.  In both
  cases the first function only gets direct definitions and the second
  uses inheritance.
  The function \code{findMethod} returns the package(s) in the search
  list (or in the packages specified by the \code{where} argument) that
  contain a method for this function and signature.

  The other functions are support functions: see the details below.
}
\usage{
getMethod(f, signature=character(), where, optional=FALSE, mlist)

findMethod(f, signature, where)

getMethods(f, where)

existsMethod(f, signature = character(), where)

hasMethod(f, signature=character(), where)

selectMethod(f, signature, optional = FALSE, useInherited = TRUE,
             mlist = (if (is.null(fdef)) NULL else
                      getMethodsForDispatch(f, fdef)),
             fdef = getGeneric(f, !optional))

MethodsListSelect(f, env, mlist, fEnv, finalDefault, evalArgs,
                  useInherited, fdef, resetAllowed)

}
\arguments{
  \item{f}{ The character-string name of the generic function.

    }
  \item{signature}{ The signature of classes to match to the arguments
    of \code{f}.  See the details below.

    For \code{selectMethod}, the signature can optionally be an
    environment with classes assigned to the names of the corresponding
    arguments.  Note:  the names correspond to the names of the classes, \emph{not}
    to the objects supplied in a call to the generic function.  (You
    are not likely to find this approach convenient, but it is used
    internally and is marginally more efficient.)
  }
  \item{where}{ The position or environment in which to look for the method(s):  by default,
      anywhere in
    the current search list.}
  \item{optional}{ If the selection does not produce a unique result,
    an error is generated, unless this argument is \code{TRUE}.  In that
    case, the value returned is either a \code{MethodsList} object, if
    more than one method matches this signature, or \code{NULL} if no
    method matches.}
  \item{mlist}{Optionally, the list of methods in which to search.  By
    default, the function finds the methods for the corresponding
    generic function.  To restrict the search to a particular package
    or environment, e.g., supply this argument as
    \code{getMethodsMetaData(f,where)}.  For \code{selectMethod}, see
    the discussion of argument \code{fdef}.
    }

  \item{fdef}{In \code{selectMethod}, the \code{MethodsList} object
    and/or the generic function object can be explicitly supplied.  (Unlikely to be used, except in the
    recursive call that finds matches to more than one argument.)}

  \item{env}{The environment in which argument evaluations are done in
    \code{MethodsListSelect}.  Currently must be supplied, but should
    usually be \code{sys.frame(sys.parent())} when calling the function
    explicitly for debugging purposes.}
  \item{fEnv, finalDefault, evalArgs, useInherited, resetAllowed}{ Internal-use
    arguments for the function's environment, the method to use as
    the overall default, whether to evaluate arguments, which
    arguments should use inheritance, and whether the cached methods
    are allowed to be reset.}
}
\details{
  The \code{signature} argument specifies classes, in an extended
    sense, corresponding to formal arguments of the generic function.
    As supplied, the argument may be a vector of strings identifying
    classes, and may be named or not.  Names, if supplied, match the
    names of those formal arguments included in the signature of the
    generic.  That signature is normally all the arguments except
    \dots.  However, generic functions can be specified with only a
    subset of the arguments permitted, or with the signature taking
    the arguments in a different order.

    It's a good idea to name the arguments in the signature to avoid
    confusion, if you're dealing with a generic that does something
    special with its signature.   In any case, the elements of the
    signature are matched to the formal signature by the same rules
    used in matching arguments in function calls (see
    \code{\link{match.call}}).

    The strings in the signature may be class names, \code{"missing"}
    or \code{"ANY"}.  See \link{Methods} for the meaning of these in
    method selection.  Arguments not supplied in the signature
    implicitly correspond to class \code{"ANY"}; in particular, giving
    an empty signature means to look for the default method.

  A call to \code{getMethod} returns the method for a particular
  function and signature.  As with other \code{get} functions,
  argument \code{where} controls where the function looks (by default
  anywhere in the search list) and argument \code{optional} controls
  whether the function returns \code{NULL} or generates an error if
  the method is not found.  The search for the method makes no use of
  inheritance.

  The function \code{selectMethod} also looks for a method given the
  function and signature, but makes full use of the method dispatch
  mechanism; i.e., inherited methods and group generics are taken into
  account just as they would be in dispatching a method for the
  corresponding signature, with the one exception that conditional
  inheritance is not used.  Like \code{getMethod}, \code{selectMethod}
  returns \code{NULL} or generates an error if
  the method is not found, depending on the argument \code{optional}.

  The functions \code{existsMethod} and \code{hasMethod} return
  \code{TRUE} or \code{FALSE} according to whether a method is found,
  the first corresponding to \code{getMethod} (no inheritance) and the
  second to \code{selectMethod}.

  The function \code{getMethods} returns all the methods for a
  particular generic (in the form of a generic function with the
  methods information in its environment).  The function is called
  from the evaluator to merge method information, and is not intended
  to be called directly.  Note that it gets \emph{all} the visible
  methods for the specified functions.  If you want only the methods
  defined explicitly in a particular environment, use the function
  \code{\link{getMethodsMetaData}} instead.

  The function \code{MethodsListSelect} performs a full search
  (including all inheritance and group generic information: see the
  \link{Methods} documentation page for details on how this works).
  The call returns a possibly revised methods list object,
  incorporating any method found as part of the \code{allMethods}
  slot.

  Normally you won't call \code{MethodsListSelect} directly, but it is
  possible to use it for debugging purposes (only for distinctly
  advanced users!).

  Note that the statement that \code{MethodsListSelect} corresponds to the
  selection done by the evaluator is a fact, not an assertion, in the
  sense that the evaluator code constructs and executes a call to
  \code{MethodsListSelect} when it does not already have a cached method
  for this generic function and signature.  (The value returned is
  stored by the evaluator so that the search is not required next
  time.)


}
\value{
  The call to \code{selectMethod} or \code{getMethod} returns a
  \code{\link{MethodDefinition-class}} object, the selected method, if
  a unique selection exists.
  (This class extends \code{function}, so you can use the result
  directly as a function if that is what you want.)
  Otherwise an error is thrown if \code{optional} is \code{FALSE}.  If
  \code{optional} is \code{TRUE}, the value returned is \code{NULL} if
  no method matched, or a \code{MethodsList} object if multiple
  methods matched.

  The call to \code{getMethods} returns the \code{MethodsList} object
  containing all the methods requested.  If there are none,
  \code{NULL} is returned: \code{getMethods} does not generate an
  error in this case.
}
\references{
  The R package \pkg{methods} implements, with a few exceptions, the
  programming interface for classes
  and methods in the book \emph{Programming with Data} (John
  M. Chambers, Springer, 1998), in particular sections 1.6, 2.7, 2.8,
  and chapters 7 and 8.

  While the programming interface for the \pkg{methods} package follows
  the reference, the R software is an original implementation, so
  details in the reference that reflect the S4 implementation may appear
  differently in R.  Also, there are extensions to the programming
  interface developed more recently than the reference.  For a
  discussion of details and ongoing development, see the web page
  \url{http://developer.r-project.org/methodsPackage.html} and the
  pointers from that page.
}
\examples{
setGeneric("testFun", function(x)standardGeneric("testFun"))
setMethod("testFun", "numeric", function(x)x+1)
hasMethod("testFun", "numeric")
\dontrun{[1] TRUE}
hasMethod("testFun", "integer") #inherited
\dontrun{[1] TRUE}
existsMethod("testFun", "integer")
\dontrun{[1] FALSE}
hasMethod("testFun") # default method
\dontrun{[1] FALSE}
hasMethod("testFun", "ANY")
\dontrun{[1] FALSE}
\dontshow{
stopifnot(isGeneric("testFun"))
stopifnot(hasMethod("testFun", "numeric"))
stopifnot(hasMethod("testFun", "integer"))
stopifnot(!existsMethod("testFun", "integer"))
stopifnot(!hasMethod("testFun"))
stopifnot(!hasMethod("testFun", "ANY"))
removeGeneric("testFun")
}
}
\keyword{programming}
\keyword{classes}
\keyword{methods}
