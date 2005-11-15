\name{UseMethod}
\title{Class Methods}
\alias{UseMethod}
\alias{NextMethod}
\alias{S3Methods}
\alias{.isMethodsDispatchOn}
\description{
  \R possesses a simple generic function mechanism which can be used for
  an object-oriented style of programming.  Method dispatch takes place
  based on the class of the first argument to the generic function or on
  the object supplied as an argument to \code{UseMethod} or \code{NextMethod}.
}
\usage{
UseMethod(generic, object)
NextMethod(generic = NULL, object = NULL, \dots)
}
\arguments{
  \item{generic}{a character string naming a function. Required for
    \code{UseMethod}.}
  \item{object}{an object whose class will determine the method to be
    dispatched.  Defaults to the first argument of the enclosing function.}
  \item{\dots}{further arguments to be passed to the method.}
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

  When a generic function \code{fun} is applies to an object with class
  attribute \code{c("first", "second")}, the system searches for a
  function called \code{fun.first} and, if it finds it, applied it to
  the object.  If no such function is found a function called
  \code{fun.second} is tried.  If no class name produces a suitable
  function, the function \code{fun.default} is used, if it exists, or an
  error results.
  

  Function \code{\link{methods}} can be used to find out about the
  methods for a particular generic function or class.

  Now for some obscure details that need to appear somewhere.  These
  comments will be slightly different than those in Appendix A of the
  White S Book. \code{UseMethod} creates a \dQuote{new} function call with
  arguments matched as they came in to the generic.  Any local variables
  defined before the call to \code{UseMethod} are retained (unlike S).  Any
  statements after the call to \code{UseMethod} will not be evaluated as
  \code{UseMethod} does not return.  \code{UseMethod} can be called with
  more than two arguments: a warning will be given and additional
  arguments ignored. (They are not completely ignored in S.)  If it is
  called with just one argument, the class of the first argument of the
  enclosing function is used as \code{object}: unlike S this is the first
  actual argument passed and not the current value of the object of that
  name.

  \code{UseMethod} is a primitive function so positional matching is
  used and names of supplied arguments are ignored.

  \code{NextMethod} invokes the next method (determined by the
  class).  It does this by creating a special call frame for that
  method.  The arguments will be the same in number, order and name as
  those to the current method but their values will be promises to
  evaluate their name in the current method and environment.  Any
  arguments matched to \code{\dots} are handled specially.  They are
  passed on as the promise that was supplied as an argument to the
  current environment. (S does this differently!)  If they have been
  evaluated in the current (or a previous environment) they remain
  evaluated.

  \code{NextMethod} should not be called except in methods called by
  \code{UseMethod}. In particular it will not work inside anonymous
  calling functions (eg \code{get("print.ts")(AirPassengers)}).

  Name spaces can register methods for generic functions.  To support
  this, \code{UseMethod} and \code{NextMethod} search for methods in
  two places: first in the environment in which the generic function
  is called, and then in the registration data base for the
  environment in which the generic is defined (typically a name space).
  So methods for a generic function need to either be available in the
  environment of the call to the generic, or they must be registered.
  It does not matter whether they are visible in the environment in
  which the generic is defined.
}
\note{
  This scheme is called \emph{S3} (S version 3).  For new projects,
  it is recommended to use the more flexible and robust \emph{S4} scheme
  provided in the \pkg{methods} package. 

  The function \code{.isMethodsDispatchOn()} returns \code{TRUE} if
  the S4 method dispatch has been turned on in the evaluator. It is
  meant for \R internal use only.
}
\section{Warning}{
  Prior to \R 2.1.0 \code{UseMethod} accepted a call with no arguments
  and tried to deduce the generic from the context.  This was
  undocumented on the help page.  It is allowed but
  \sQuote{strongly discouraged} in S-PLUS, and is no longer allowed in \R.
}
\seealso{
  \code{\link{methods}}, \code{\link[base]{class}}, \code{\link{getS3method}}
}
\references{
  Chambers, J. M. (1992)
  \emph{Classes and methods: object-oriented programming in S.}
  Appendix A of \emph{Statistical Models in S}
  eds J. M. Chambers and T. J. Hastie, Wadsworth \& Brooks/Cole.
}
\keyword{methods}
