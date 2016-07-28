% File src/library/methods/man/getMethod.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2016 R Core Team
% Distributed under GPL 2 or later

\name{getMethod}
\title{Get or Test for the Definition of a Method}
\alias{getMethod}
\alias{findMethod}
\alias{existsMethod}
\alias{selectMethod}
\alias{hasMethod}
\description{
  Functions to look for a method corresponding to a given generic function and signature.
  The functions \code{getMethod} and \code{selectMethod} return the method; the functions
  \code{existsMethod} and \code{hasMethod} test for its existence.  In both
  cases the first function only gets direct definitions and the second
  uses inheritance.  In all cases, the search is in the generic function itself or in
  the package/environment specified by argument \code{where}.

  The function \code{findMethod} returns the package(s) in the search
  list (or in the packages specified by the \code{where} argument) that
  contain a method for this function and signature.
}
\usage{
existsMethod(f, signature = character(), where)

  findMethod(f, signature, where)

   getMethod(f, signature = character(), where, optional = FALSE, mlist, fdef)

   hasMethod(f, signature = character(), where)

selectMethod(f, signature, optional = FALSE, useInherited =,
             mlist = , fdef = , verbose = , doCache = )
}
\arguments{
  \item{f}{a generic function or the character-string name of one.}
  \item{signature}{the signature of classes to match to the arguments
    of \code{f}.  See the details below.}
  \item{where}{the position or environment in which to look for the
    method(s):  by default, the table of methods defined in the generic
    function itself is used.}

  \item{optional}{if the selection in \code{selectMethod} does not find
    a valid method an error is generated, unless \code{optional} is
    true.  In that case, the value returned is \code{NULL} if no method
    matches.}
  \item{mlist, fdef, useInherited, verbose, doCache}{optional arguments
    to  \code{getMethod} and \code{selectMethod} for internal use.  Avoid
    these: some will work as expected and others will not, and none of
    them is required for normal use of the functions.}
}
\details{
  The \code{signature} argument specifies classes, corresponding to
  formal arguments of the generic function; to be precise, to the
  \code{signature} slot of the generic function object.  The argument
  may be a vector of strings identifying classes, and may be named or
  not.  Names, if supplied, match the names of those formal arguments
  included in the signature of the generic.  That signature is normally
  all the arguments except \dots.  However, generic functions can be
  specified with only a subset of the arguments permitted, or with the
  signature taking the arguments in a different order.

  It's a good idea to name the arguments in the signature to avoid
  confusion, if you're dealing with a generic that does something
  special with its signature.  In any case, the elements of the
  signature are matched to the formal signature by the same rules used
  in matching arguments in function calls (see
  \code{\link{match.call}}).

  The strings in the signature may be class names, \code{"missing"} or
  \code{"ANY"}.  See \link{Methods} for the meaning of these in method
  selection.  Arguments not supplied in the signature implicitly
  correspond to class \code{"ANY"}; in particular, giving an empty
  signature means to look for the default method.

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

}
\value{
  The call to \code{selectMethod} or \code{getMethod} returns  the selected method, if
 one is found.
  (This class extends \code{function}, so you can use the result
  directly as a function if that is what you want.)
  Otherwise an error is thrown if \code{optional} is \code{FALSE}  and  \code{NULL} is returned if
  \code{optional} is \code{TRUE}.

The returned method object is a
  \code{\linkS4class{MethodDefinition}} object, \emph{except} that the default method for a primitive function is required to be the primitive itself.
Note therefore that the only reliable test that the search failed is \code{is.null()}.

}
\references{
 Chambers, John M. (2008)
 \emph{Software for Data Analysis: Programming with R}
  Springer.  (For the R version.)

 Chambers, John M. (1998)
 \emph{Programming with Data}
 Springer (For the original S4 version.)
}
\seealso{\code{\link{Methods}} for the details of method
  selection; \code{\link{GenericFunctions}} for other functions
  manipulating methods and generic function objects;
  \code{\linkS4class{MethodDefinition}} for the class that represents
  method definitions.}
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
stopifnot(isGeneric("testFun"),
          hasMethod("testFun", "numeric"),
          hasMethod("testFun", "integer"),
          !existsMethod("testFun", "integer"),
          !hasMethod("testFun"),
          !hasMethod("testFun", "ANY") )
removeGeneric("testFun")
}
}
\keyword{programming}
\keyword{classes}
\keyword{methods}
