\name{methods}
\title{Class Methods}
\usage{
UseMethod(generic, object )
NextMethod(generic, object, ...)
methods(generic.function, class)
}
\alias{UseMethod}
\alias{NextMethod}
\alias{methods}
\description{
  \R possesses a simple generic function mechanism which can be used for
  an object-oriented style of programming.  Method despatch takes place
  based on the class of the first argument to the generic function or on 
  the object supplied as an argument to \code{UseMethod} or \code{NextMethod}.
}
\details{
  An \R ``object'' is a data object which has a \code{class} attribute.
  A class attribute is a vector of character strings giving the names of
  the classes which the object ``inherits'' from.  When a generic
  function \code{fun} is applied to an object with class attribute
  \code{c("first", "second")}, the system searches for a function called
  \code{fun.first} and, if it finds it, applied it to the object.  If no
  such function is found a function called \code{fun.second} is tried.
  If no class name produces a suitable function, the function
  \code{fun.default} is used.

  \code{methods} can be used to find out about the methods for a
  particular generic function or class.  See the examples below for
  details.

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
  arguments matched to \code{...} are handled specially.  They are
  passed on as the promise that was supplied as an argument to the
  current environment. (S does this differently!)  If they have been
  evaluated in the current (or a previous environment) they remain
  evaluated.
}
\note{
  The \code{methods} function was written by Martin Maechler.
}
\seealso{
  \code{\link{class}}
}
\examples{
methods(summary)

methods(print)

methods(class = data.frame)

methods("[")#- does not list the C-internal ones...
}
\keyword{methods}
