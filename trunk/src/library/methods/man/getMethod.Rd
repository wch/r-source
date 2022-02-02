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
  The function \code{selectMethod()} returns the method that
  would be selected for a call to function \code{f} if the arguments had
  classes as specified by \code{signature}.  Failing to find a method
  is an error, unless argument \code{optional = TRUE}, in which case
  \code{NULL} is returned.

  The function \code{findMethod()} returns a list of
  environments that contain a method for the specified function and signature; by
  default, these are a subset of the packages in the current search
  list.  See section \dQuote{Using \code{findMethod()}} for details.

The function \code{getMethod()} returns the method corresponding to the
function and signature supplied similarly to \code{selectMethod}, but
without using inheritance or group generics.

The functions  \code{hasMethod()}  and
\code{existsMethod()} test whether \code{selectMethod()} or
\code{getMethod()}, respectively, finds a matching method.


}
\usage{
  selectMethod(f, signature, optional = FALSE, useInherited =,
             mlist = , fdef = , verbose = , doCache = )

  findMethod(f, signature, where)

  getMethod(f, signature = character(), where, optional = FALSE,
             mlist, fdef)

  existsMethod(f, signature = character(), where)

  hasMethod(f, signature = character(), where)
}
\arguments{
  \item{f}{a generic function or the character-string name of one.}
  \item{signature}{the signature of classes to match to the arguments
    of \code{f}.  See the details below.}
  \item{where}{the environment in which to look for the
    method(s).  By default, if the call comes from the command line, the table of methods defined in the generic
    function itself is used, except for \code{findMethod} (see the
    section below).}

  \item{optional}{if the selection in \code{selectMethod} does not find
    a valid method an error is generated, unless \code{optional} is
    \code{TRUE},  in which case the value returned is \code{NULL}.}

  \item{mlist, fdef, useInherited, verbose, doCache}{optional arguments
    to  \code{getMethod} and \code{selectMethod} for internal use.  Avoid
    these: some will work as expected and others will not, and none of
    them is required for normal use of the functions.  But see the
    section \dQuote{Methods for \code{as()}} for nonstandard inheritance.}
}
\section{Using \code{findMethod()}}{
As its name suggests, this function is intended to behave like
\code{\link{find}}, which produces a list of the packages on the
current search list which have, and have exported, the object named.
That's what \code{findMethod} does also, by default.  The
\dQuote{exported} part in this case means that the package's namespace
has an \code{exportMethods} directive for this generic function.

An important distinction is that the absence of such a directive does
not prevent methods from the package from being called once the
package is loaded.  Otherwise, the code in the package could not use
un-exported methods.

So, if your question is whether loading package \code{thisPkg} will define a
method for this function and signature, you need to ask that question
about the namespace of the package:

\code{findMethod(f, signature, where = asNamespace("thisPkg"))}

If the package did not export the method, attaching it and calling
\code{findMethod} with no \code{where} argument will not find the
method.

Notice also that the length of the signature must be what the
corresponding package used.  If \code{thisPkg} had only methods for
one argument, only length-1 signatures will match (no trailing
\code{"ANY"}), even if another currently loaded package had signatures
with more arguments.
}
\section{Methods for \code{as()}}{
The function \code{\link{setAs}} allows packages to define methods for
coercing one class of objects to another class.  This works internally
by defining methods for the generic function \code{\link{coerce}(from,
to)},
which can not be called directly.

The \R evaluator selects
methods for this purpose using a different form of inheritance.  While
methods can be inherited for the object being coerced, they cannot
inherit for the target class, since the result would not be a valid
object from that class.
If you want to
examine the selection procedure, you must supply the optional argument
\code{useInherited = c(TRUE, FALSE)} to \code{selectMethod}.
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
  \code{"ANY"}.  See \link{Methods_Details} for the meaning of these in method
  selection.  Arguments not supplied in the signature implicitly
  correspond to class \code{"ANY"}; in particular, giving an empty
  signature means to look for the default method.

  A call to \code{getMethod} returns the method for a particular
  function and signature.   The search for the method makes no use of
  inheritance.

  The function \code{selectMethod} also looks for a method given the
  function and signature, but makes full use of the method dispatch
  mechanism; i.e., inherited methods and group generics are taken into
  account just as they would be in dispatching a method for the
  corresponding signature, with the one exception that conditional
  inheritance is not used.  Like \code{getMethod}, \code{selectMethod}
  returns \code{NULL} or generates an error if
  the method is not found, depending on the argument \code{optional}.

Both \code{selectMethod} and \code{getMethod} will normally use the
current version of the generic function in the R session, which has a
table of the methods obtained from all the packages loaded in the
session. Optional arguments can cause a search for the generic function from a
specified environment, but this is rarely a useful idea.  In contrast,
\code{findMethod} has a different default and the optional
\code{where=} argument may be needed.  See the  section \dQuote{Using
  \code{findMethod()}}.

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
Note therefore that the only reliable test that the search failed is
\code{is.null()}.

The returned value of \code{findMethod} is a list of
environments in which a corresponding method was found; that is, a
table of methods including the one specified.

}
\references{
 Chambers, John M. (2016)
 \emph{Extending R},
  Chapman & Hall.
(Chapters 9 and 10.)

 Chambers, John M. (2008)
 \emph{Software for Data Analysis: Programming with R}
  Springer. (Section 10.6 for some details of method selection.)
}
\seealso{\code{\link{Methods_Details}} for the details of method
  selection; \code{\link{GenericFunctions}} for other functions
  manipulating methods and generic function objects;
  \code{\linkS4class{MethodDefinition}} for the class that represents
  method definitions.}
\examples{
testFun <-  function(x)x
setGeneric("testFun")
setMethod("testFun", "numeric", function(x)x+1)

hasMethod("testFun", "numeric") # TRUE

hasMethod("testFun", "integer") #TRUE, inherited

existsMethod("testFun", "integer") #FALSE

hasMethod("testFun") # TRUE, default method

hasMethod("testFun", "ANY")

\dontshow{
## Verify the example
stopifnot(isGeneric("testFun"),
          hasMethod("testFun", "numeric"),
          hasMethod("testFun", "integer"),
          !existsMethod("testFun", "integer"),
          hasMethod("testFun"),
          hasMethod("testFun", "ANY") )
removeGeneric("testFun")
}
}
\keyword{programming}
\keyword{classes}
\keyword{methods}
