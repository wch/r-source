\name{getMethod}
\alias{getMethod}
\alias{findMethod}
\alias{getMethods}
\alias{selectMethod}
\alias{MethodsListSelect}
\title{ Get the Definition of a Method }
\description{
  A call to \code{getMethod} returns the method corresponding a
  particular generic function and signature; a call to \code{findMethod}
  returns the vector of package names in which a matching method is found.

  The functions \code{MethodsListSelect} and \code{selectMethod} select
  methods using an environment in which arguments to the function have
  been assigned (they differ in the treatment of inheritance).  Neither
  is intended to be called directly, except perhaps in debugging.  The
  function \code{getMethods} returns \emph{all} the methods for the
  generic function.

  The function \code{findMethod} returns the package(s) in the search
  list (or in the packages specified by the \code{where} argument) that
  contain a method for this function and signature.
}
\usage{
getMethod(f, signature=character(), where, optional=FALSE)

findMethod(f, signature, where)

getMethods(f, where=-1)

MethodsListSelect(f, env, mlist, fEnv, finalDefault, evalArgs, useInherited)
selectMethod(f, signature, optional=FALSE, useInherited, mlist=getMethods(f))
}
\arguments{
  \item{f}{ The character-string name of the generic function.

    In \code{getMethods} only, this argument may be a function
    definition, in which case the special methods list object, if any,
    stored in the environment of the function is returned.  (This usage
    is largely for internal purposes; you aren't likely to have such a
    function definition for direct use.)}
  \item{signature}{ The signature of classes to match to the arguments
    of \code{f}.  The vector of strings for the classes should be named,
    and the names must match formal argument names of \code{f}.  If not
    named, the signature is assumed to apply to the arguments of
    \code{f} in order, but note below for \code{selectMethod}.

    For \code{selectMethod}, the signature can optionally be an
    environment with classes assigned to the names of the corresponding
    arguments.  If the signature is not an environment, it must
    currently be named.}
  \item{where}{ The position or environment in which to look for the method:  by default,
      anywhere in
    the current search list.}
  \item{optional}{ If the selection does not produce a unique result,
    an error is generated, unless this argument is \code{TRUE}.  In that
    case, the value returned is either a \code{MethodsList} object, if
    more than one method matches this signature, or \code{NULL} if no
    method matches.}

  \item{mlist}{In \code{selectMethod}, the \code{MethodsList} object
    can be explicitly supplied.  (Unlikely to be used, except in the
    recursive call that finds matches to more than one argument.)}

  \item{env}{The environment in which argument evaluations are done in
    \code{MethodsListSelect}.  Currently must be supplied, but should
    usually be \code{sys.frame(sys.parent())} when calling the function
    explicitly for debugging purposes.}
  \item{fEnv, finalDefault, evalArgs, useInherited}{ Internal-use
    arguments for the function's environment, the method to use as
    the overall default, whether to evaluate arguments, and which
    arguments should use inheritance.}
}
\details{
  A call to \code{getMethod} returns the method for a particular
  function and signature.  As with other \code{get} functions,
  argument \code{where} controls where the function looks (by default
  anywhere in the search list) and argument \code{optional} controls
  whether the function returns \code{NULL} or generates an error if
  the method is not found.  The search for the method makes no use of
  inheritance.

  The function \code{getMethods} returns all the methods for a
  particular generic (in the form of a generic function with the
  methods information in its environment).  The function is called
  from the evaluator to merge method information, and is not intended
  to be called directly.

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

  The \code{selectMethod} function is similar to
  \code{MethodsListSelect} (and calls it) but differs in that it
  is intended to be used with control over which arguments are allowed
  to use inheritance.  The restriction is needed to make sensible
  method selection for coerce methods (the \code{\link{as}} function).
}
\value{
  The call to \code{selectMethod} or \code{getMethod} returns a
  function object, the selected method, if a unique selection exists.
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
  The web page \url{http://www.omegahat.org/RSMethods/index.html} is the
  primary documentation.

  The functions in this package implement a facility for classes and
  methods as described in \emph{Programming with Data} (John
  M. Chambers, Springer, 1998).  See this book for further details and
  examples.
}
\author{
  John Chambers
}
\keyword{programming}
\keyword{classes}
\keyword{methods}
