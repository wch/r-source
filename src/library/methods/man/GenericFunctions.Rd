% File src/library/methods/man/GenericFunctions.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2016 R Core Team
% Distributed under GPL 2 or later

\name{GenericFunctions}
\alias{GenericFunctions}
\alias{isGeneric}
\alias{isGroup}
\alias{removeGeneric}
\alias{getGenerics}
\alias{dumpMethod}
\alias{findFunction}
\alias{dumpMethods}
\alias{removeMethods}
\alias{signature}
\alias{setReplaceMethod}
%% FIXME:  These are basically not documented at all:
%% -----  isGroup, setReplaceMethod
\title{Tools for Managing Generic Functions}
\description{
  The functions documented here manage collections of methods associated
  with a generic function, as well as providing information about the
  generic functions themselves.
}
\usage{
isGeneric(f, where, fdef, getName = FALSE)
isGroup(f, where, fdef)
removeGeneric(f, where)

dumpMethod(f, signature, file, where, def)
findFunction(f, generic = TRUE, where = topenv(parent.frame()))
dumpMethods(f, file, signature, methods, where)
signature(\dots)

removeMethods(f, where = topenv(parent.frame()), all = missing(where))

setReplaceMethod(f, \dots, where = topenv(parent.frame()))

getGenerics(where, searchForm = FALSE)
}
\section{Summary of Functions}{
  \describe{
    \item{\code{isGeneric}:}{
      Is there a function named \code{f}, and if so, is it a generic?

      The \code{getName} argument allows a function to find the name
      from a function definition.  If it is \code{TRUE} then the name of
      the generic is returned, or \code{FALSE} if this is not a generic
      function definition.

      The behavior of \code{isGeneric} and \code{\link{getGeneric}} for
      primitive functions is slightly different.  These functions don't
      exist as formal function objects (for efficiency and historical
      reasons), regardless of whether methods have been defined for
      them.  A call to \code{isGeneric} tells you whether methods have
      been defined for this primitive function, anywhere in the current
      search list, or in the specified position \code{where}.  In
      contrast, a call to \code{\link{getGeneric}} will return what the
      generic for that function would be, even if no methods have been
      currently defined for it.
    }

    \item{\code{removeGeneric}, \code{removeMethods}:}{
      Remove all the methods for the generic function of this
      name.  In addition, \code{removeGeneric} removes the function
      itself; \code{removeMethods} restores the non-generic function
      which was the default method.   If there was no default method,
      \code{removeMethods} leaves a generic function with no methods.
    }
    \item{\code{standardGeneric}:}{
      Dispatches a method from the current function call for the generic
      function \code{f}.  It is an error to call
      \code{standardGeneric} anywhere except in the body of the
      corresponding generic function.

      Note that \code{\link{standardGeneric}} is a primitive function in
      the \pkg{base} package
      for efficiency % << MM: this is just my guess, what *are* the reasons?
      reasons, but rather documented here where it belongs naturally.
    }
    \item{\code{dumpMethod}:}{
      Dump the method for this generic function and signature.
    }
    \item{\code{findFunction}:}{
      return a list of either the positions on the search list, or the
      current top-level environment, on which a function object
      for \code{name} exists.  The returned value is \emph{always} a
      list, use the first element to access the first visible version
      of the function.  See the example.

      \emph{NOTE:} Use this rather than \code{\link{find}} with
      \code{mode="function"}, which is not as meaningful, and has a few
      subtle bugs from its use of regular expressions.  Also,
      \code{findFunction} works correctly in the code for a package
      when attaching the package via a call to \code{\link{library}}.
    }
    \item{\code{dumpMethods}:}{
      Dump all the methods for this generic.
    }
    \item{\code{signature}:}{
      Returns a named list of classes to be matched to arguments of a
      generic function.
    }
    \item{\code{getGenerics}:}{returns the names of the generic
      functions that have methods defined on \code{where}; this
      argument can be an environment or an index into the search
      list.  By default, the whole search list is used.

      The methods definitions are stored with
      package qualifiers; for example, methods for function
      \code{"initialize"} might refer to two different functions
      of that name, on different packages.  The package names
      corresponding to the method list object are contained in the
      slot \code{package} of the returned object.  The form of
      the returned name can be plain (e.g., \code{"base"}), or in
      the form used in the search list (\code{"package:base"})
      according to the value of \code{searchForm}}

  }
}

\arguments{
  \item{f}{ The character string naming the function. }
  \item{where}{ The environment, namespace, or search-list position
  from which to search for objects.  By default, start at the
  top-level environment of the calling function, typically the global
  environment (i.e., use the search list), or the namespace of a
  package from which the call came.  It is important to supply this
  argument when calling any of these functions indirectly.  With
  package namespaces, the default is likely to be wrong in such calls.}

  \item{signature}{ The class signature of the relevant method.  A
    signature is a named or unnamed vector of character strings.  If
    named, the names must be formal argument names for the generic
    function.  Signatures are matched to the arguments specified in
    the signature slot of the generic function (see the Details
    section of the \code{\link{setMethod}} documentation).

  The \code{signature} argument to \code{dumpMethods} is ignored (it
  was used internally in previous implementations).}

  \item{file}{ The file or connection on which to dump method definitions. }

  \item{def}{ The function object defining the method; if omitted, the
    current method definition corresponding to the signature. }

  \item{\dots}{Named or unnamed arguments to form a signature.}

  \item{generic}{In testing or finding functions, should generic
    functions be included.  Supply as \code{FALSE} to get only
    non-generic functions.}

  \item{fdef}{Optional, the generic function definition.

   Usually omitted in calls to \code{isGeneric}}

  \item{getName}{If \code{TRUE}, \code{isGeneric} returns the name of
    the generic.  By default, it returns \code{TRUE}. }
  \item{methods}{
    The methods object containing the methods to be dumped.  By default,
    the methods defined for this generic (optionally on the specified
    \code{where} location).
  }
  \item{all}{in \code{removeMethods}, logical indicating if all
    (default) or only the first method found should be removed.}
  \item{searchForm}{In \code{getGenerics}, if \code{TRUE}, the
    \code{package} slot of the returned result is in the form used
    by \code{search()}, otherwise as the simple package name (e.g,
    \code{"package:base"} vs \code{"base"}).
  }
}

\section{Details}{
  \describe{
    \item{\code{setGeneric}:}{
      If there is already a non-generic function of this name, it will
      be used to define the generic unless \code{def} is supplied, and
      the current function will become the default method for the
      generic.

      If \code{def} is supplied, this defines the generic function, and
      no default method will exist (often a good feature, if the
      function should only be available for a meaningful subset of all
      objects).

      Arguments \code{group} and \code{valueClass} are retained for
      consistency with S-Plus, but are currently not used.
    }
    \item{\code{isGeneric}:}{
      If the \code{fdef} argument is supplied, take this as the
      definition of the generic, and test whether it is really a
      generic, with \code{f} as the name of the generic.  (This argument
      is not available in S-Plus.)
    }
    \item{\code{removeGeneric}:}{
      If \code{where} supplied, just remove the version on this element
      of the search list; otherwise, removes the first version
      encountered.
    }
    \item{\code{standardGeneric}:}{
      Generic functions should usually have a call to
      \code{standardGeneric} as their entire body.  They can, however,
      do any other computations as well.

      The usual \code{setGeneric} (directly or through calling
      \code{setMethod}) creates a function with a call to
      \code{standardGeneric}.
    }
    \item{\code{dumpMethod}:}{
      The resulting source file will recreate the method.
    }
    \item{\code{findFunction}:}{
      If \code{generic} is \code{FALSE}, ignore generic functions.
    }
    \item{\code{dumpMethods}:}{
      If \code{signature} is supplied only the methods matching this
      initial signature are dumped.  (This feature is not found in
      S-Plus:  don't use it if you want compatibility.)
    }
    \item{\code{signature}:}{
      The advantage of using \code{signature} is to provide a check on
      which arguments you meant, as well as clearer documentation in
      your method specification.  In addition, \code{signature} checks
      that each of the elements is a single character string.
    }
    \item{\code{removeMethods}:}{
      Returns \code{TRUE} if \code{f} was a generic function,
      \code{FALSE} (silently) otherwise.

      If there is a default method, the function will be re-assigned as
      a simple function with this definition.
      Otherwise, the generic function remains but with no methods (so
      any call to it will generate an error).  In either case, a
      following call to \code{setMethod} will consistently
      re-establish the same generic function as before.
    }
  }
}
\references{
 Chambers, John M. (2008)
 \emph{Software for Data Analysis: Programming with R}
  Springer.  (For the R version.)

 Chambers, John M. (1998)
 \emph{Programming with Data}
 Springer (For the original S4 version.)
}
\seealso{
  \code{\link{getMethod}} (also for \code{selectMethod}),
  \code{\link{setGeneric}},
  \code{\link{setClass}},
  \code{\link{showMethods}}
}
\examples{
require(stats) # for lm

## get the function "myFun" -- throw an error if 0 or > 1 versions visible:
findFuncStrict <- function(fName) {
  allF <- findFunction(fName)
  if(length(allF) == 0)
    stop("No versions of ",fName," visible")
  else if(length(allF) > 1)
    stop(fName," is ambiguous: ", length(allF), " versions")
  else
    get(fName, allF[[1]])
}

try(findFuncStrict("myFun"))# Error: no version
lm <- function(x) x+1
try(findFuncStrict("lm"))#    Error: 2 versions
findFuncStrict("findFuncStrict")# just 1 version
rm(lm)

\dontshow{
## because nosegfault runs standardGeneric w/o the methods package, nothing
## really gets tested.  The following check that it catches some errors
mustDie <- function(expr)
   stopifnot(is(tryCatch(expr, error=function(e)e), "error"))

mustDie(standardGeneric()) # 3 tests of requiring a single string
mustDie(standardGeneric(NULL))
mustDie(standardGeneric(""))
mustDie(standardGeneric("notAGenericFunction"))
mustDie(standardGeneric("show"))  # a generic, but not called from its body
}

## method dumping ------------------------------------

setClass("A", slots = c(a="numeric"))
setMethod("plot", "A", function(x,y,...){ cat("A meth\n") })
dumpMethod("plot","A", file="")
\dontrun{
setMethod("plot", "A",
function (x, y, ...)
{
    cat("AAAAA\n")
}
)
}%dont
tmp <- tempfile()
dumpMethod("plot","A", file=tmp)
## now remove, and see if we can parse the dump
stopifnot(removeMethod("plot", "A"))
source(tmp)
stopifnot(is(getMethod("plot", "A"), "MethodDefinition"))

## same with dumpMethods() :
setClass("B", contains="A")
setMethod("plot", "B", function(x,y,...){ cat("B ...\n") })
dumpMethods("plot", file=tmp)
stopifnot(removeMethod("plot", "A"),
          removeMethod("plot", "B"))
source(tmp)
stopifnot(is(getMethod("plot", "A"), "MethodDefinition"),
          is(getMethod("plot", "B"), "MethodDefinition"))
}
\keyword{programming}
\keyword{classes}
\keyword{methods}
