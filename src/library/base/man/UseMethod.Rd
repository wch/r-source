\name{UseMethod}
\title{Class Methods}
\alias{UseMethod}
\alias{NextMethod}
\alias{S3Methods}
\alias{.Method}
\alias{.Generic}
\alias{.Class}
\description{
  \R possesses a simple generic function mechanism which can be used for
  an object-oriented style of programming.  Method dispatch takes place
  based on the class(es) of the first argument to the generic function or of
  the object supplied as an argument to \code{UseMethod} or \code{NextMethod}.
}
\usage{
UseMethod(generic, object)

NextMethod(generic = NULL, object = NULL, \dots)
}
\arguments{
  \item{generic}{a character string naming a function (and not a
    built-in operator).  Required for \code{UseMethod}.}
  \item{object}{for \code{UseMethod}: an object whose class will
    determine the method to be dispatched.  Defaults to the first
    argument of the enclosing function.}
  \item{\dots}{further arguments to be passed to the next method.}
}
\details{
  An \R \dQuote{object} is a data object which has a \code{class} attribute.
  A class attribute is a character vector giving the names of
  the classes which the object \dQuote{inherits} from.
  If the object does not have a class attribute, it has an implicit
  class.  Matrices and arrays have class \code{"matrix"}
  or\code{"array"} followed by the class of the underlying vector.
  Most vectors have class the result of \code{\link{mode}(x)}, except
  that integer vectors have class \code{c("integer", "numeric")} and
  real vectors have class \code{c("double", "numeric")}.

  When a function calling \code{UseMethod("fun")} is applied to an
  object with class attribute \code{c("first", "second")}, the system
  searches for a function called \code{fun.first} and, if it finds it,
  applies it to the object.  If no such function is found a function
  called \code{fun.second} is tried.  If no class name produces a
  suitable function, the function \code{fun.default} is used, if it
  exists, or an error results.

  Function \code{\link{methods}} can be used to find out about the
  methods for a particular generic function or class.

  \code{UseMethod} is a primitive function so positional matching is
  used and names of supplied arguments are ignored.  It is not the only
  means of dispatch of methods, for there are \link{internal generic}
  functions.

  \code{NextMethod} invokes the next method (determined by the
  class vector, either of the object supplied to the generic, or of
  the first argument to the function containing \code{NextMethod} if a
  method was invoked directly).  Normally \code{NextMethod} is used with
  only one argument, \code{generic}, but if further arguments are
  supplied these modify the call to the next method.

  \code{NextMethod} should not be called except in methods called by
  \code{UseMethod} or from internal generics (see
  \link{InternalGenerics}).  In particular it will not work inside
  anonymous calling functions (e.g. \code{get("print.ts")(AirPassengers)}).

  Name spaces can register methods for generic functions.  To support
  this, \code{UseMethod} and \code{NextMethod} search for methods in
  two places: first in the environment in which the generic function
  is called, and then in the registration data base for the
  environment in which the generic is defined (typically a name space).
  So methods for a generic function need to be available in the
  environment of the call to the generic, or they must be registered.
  (It does not matter whether they are visible in the environment in
  which the generic is defined.)
}
\section{Technical Details}{
  Now for some obscure details that need to appear somewhere.  These
  comments will be slightly different than those in Chambers(1992).
  (See also the draft \sQuote{R Language Definition}.)
  \code{UseMethod} creates a \dQuote{new} function call with
  arguments matched as they came in to the generic.  Any local variables
  defined before the call to \code{UseMethod} are retained (unlike S).  Any
  statements after the call to \code{UseMethod} will not be evaluated as
  \code{UseMethod} does not return.  \code{UseMethod} can be called with
  more than two arguments: a warning will be given and additional
  arguments ignored.  (They are not completely ignored in S.)  If it is
  called with just one argument, the class of the first argument of the
  enclosing function is used as \code{object}: unlike S this is the first
  actual argument passed and not the current value of the object of that
  name.

  \code{NextMethod} works by creating a special call frame for the next
  method.  If no new arguments are supplied, the arguments will be the
  same in number, order and name as those to the current method but
  their values will be promises to evaluate their name in the current
  method and environment.  Any named arguments matched to \code{\dots}
  are handled specially: they either replace existing arguments of the
  same name or are appended to the argument list.  They are passed on as
  the promise that was supplied as an argument to the current
  environment. (S does this differently!)  If they have been evaluated
  in the current (or a previous environment) they remain evaluated.
  (This is a complex area, and subject to change: see the draft
  \sQuote{R Language Definition}.)

  The search for methods for \code{NextMethod} is slightly different
  from that for \code{UseMethod}.   Finding no \code{fun.default} is not
  necessarily an error, as the search continues to the generic
  itself. This is to pick up an \link{internal generic} like \code{[}
  which has no separate default method, and succeeds only if the generic
  is a \link{primitive} function or a wrapper for a
  \code{\link{.Internal}} function of the same name.  (When a primitive
  is called as the default method, argument matching may not work as
  described above due to the different semantics of primitives.)

  You will see objects such as \code{.Generic}, \code{.Method}, and
  \code{.Class} used in methods.  These are set in the environment
  within which the method is evaluated by the dispatch mechanism, which
  is as follows:
  \enumerate{
    \item Find the context for the calling function (the generic): this
    gives us the unevaluated arguments for the original call.
    \item Evaluate the object (usually an argument) to be used for
    dispatch, and find a method (possibly the default method) or throw
    an error.
    \item Create an environment for evaluating the method and insert
    special variables (see below) into that environment.  Also copy any
    variables in the environment of the generic that are not formal (or
    actual) arguments.
    \item Fix up the argument list to be the arguments of the call
    matched to the formals of the method.
  }
  \code{.Generic} is a length-one character vector naming the generic function.

  \code{.Method} is a character vector (normally of length one) naming
  the method function.  (For functions in the group generic
  \code{\link[base:groupGeneric]{Ops}} it is of length two.)

  \code{.Class} is a character vector of classes used to find the next
  method.  \code{NextMethod} adds an attribute \code{"previous"} to
  \code{.Class} giving the \code{.Class} last used for dispatch, and
  shifts \code{.Class} along to that used for dispatch.

  \code{.GenericCallEnv} and \code{.GenericDefEnv} are the environments
  of the call to be generic and defining the generic respectively.  (The
  latter is used to find methods registered for the generic.)

  Note that \code{.Class} is set when the generic is called, and is
  unchanged if the class of the dispatching argument is changed in a
  method.  It is possible to change the method that \code{NextMethod}
  would dispatch by manipulating \code{.Class}, but \sQuote{this is not
    recommended unless you understand the inheritance mechanism
    thoroughly} (Chambers \& Hastie, 1992, p. 469).
}
\note{
  This scheme is called \emph{S3} (S version 3).  For new projects,
  it is recommended to use the more flexible and robust \emph{S4} scheme
  provided in the \pkg{methods} package.
}
\section{Historical note}{
  Prior to \R 2.1.0 \code{UseMethod} accepted a call with no arguments
  and tried to deduce the generic from the context.  This was
  undocumented on the help page.  It is allowed but
  \sQuote{strongly discouraged} in S-PLUS, and is no longer allowed in \R.
}
\seealso{
  The draft \sQuote{R Language Definition}.

  \code{\link{methods}}, \code{\link[base]{class}}, \code{\link{getS3method}}.
}
\references{
  Chambers, J. M. (1992)
  \emph{Classes and methods: object-oriented programming in S.}
  Appendix A of \emph{Statistical Models in S}
  eds J. M. Chambers and T. J. Hastie, Wadsworth \& Brooks/Cole.
}
\keyword{methods}
