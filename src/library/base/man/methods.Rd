\name{methods}
\title{Class Methods}
\alias{UseMethod}
\alias{NextMethod}
\alias{methods}
\alias{S3Methods}
\alias{.isMethodsDispatchOn}
\description{
  \R possesses a simple generic function mechanism which can be used for
  an object-oriented style of programming.  Method despatch takes place
  based on the class of the first argument to the generic function or on
  the object supplied as an argument to \code{UseMethod} or \code{NextMethod}.
}
\usage{
UseMethod(generic, object)
NextMethod(generic = NULL, object = NULL, \dots)
methods(generic.function, class)
}
\arguments{
  \item{generic}{a character string naming a function.}
  \item{object}{an object whose class will determine the method to be
    dispatched.  Defaults to the first argument of the enclosing function.}
  \item{\dots}{further arguments to be passed to the method.}
  \item{generic.function}{a generic function, or a character string naming a
    generic function.}
  \item{class}{a symbol or character string naming a class: only used if
    \code{generic.function} is not supplied.}
}
\details{
  An \R ``object'' is a data object which has a \code{class} attribute.
  A class attribute is a character vector giving the names of
  the classes which the object ``inherits'' from.
  If the object does not have a class attribute, it has an implicit
  class, \code{"matrix"}, \code{"array"} or the result of
  \code{\link{mode}(x)}.

  When a generic
  function \code{fun} is applied to an object with class attribute
  \code{c("first", "second")}, the system searches for a function called
  \code{fun.first} and, if it finds it, applied it to the object.  If no
  such function is found a function called \code{fun.second} is tried.
  If no class name produces a suitable function, the function
  \code{fun.default} is used.

  Function \code{methods} can be used to find out about the methods for
  a particular generic function or class.  The functions listed are those
  which \emph{are named like methods} and may not actually be methods
  (known exceptions are discarded in the code).  Note that the listed
  methods may not be user-visible objects, but often help will be
  available for them.  (\code{methods(class=)} only reports user-visible
  methods.)

  Now for some obscure details that need to appear somewhere.  These
  comments will be slightly different than those in Appendix A of the
  White S Book. \code{UseMethod} creates a ``new'' function call with
  arguments matched as they came in to the generic.  Any local variables
  defined before the call to \code{UseMethod} are retained (!?).  Any
  statements after the call to \code{UseMethod} will not be evaluated as
  \code{UseMethod} does not return.

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

}
\note{
  This scheme is called \emph{S3} (S version 3).  For new projects,
  it is recommended to use the more flexible and robust \emph{S4} scheme
  provided in the \code{methods} package.  Functions can have both S3
  and S4 methods, and function \code{\link[methods]{showMethods}} will
  list the S4 methods (possibly none).
  \cr
  The function \code{.isMethodsDispatchOn()} returns \code{TRUE} if the
  new S4 methods are available but is meant for \R internal use only.

  The \code{methods} function was written by Martin Maechler.
}
\seealso{
  \code{\link[base]{class}}, \code{\link{getS3method}}
}
\examples{
methods(summary)

\dontrun{methods(print)

methods(class = data.frame)

methods("[")  ##- does not list the C-internal ones...
}}
\keyword{methods}
