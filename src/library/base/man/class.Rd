\name{class}
\title{Object Classes}
\usage{
class(x)
class(x) <- names
unclass(x)
inherits(x, what, which = FALSE)
}
\alias{class}
\alias{class<-}
\alias{unclass}
\alias{inherits}
\description{
  \R possesses a simple generic function mechanism which can be used for
  an object-oriented style of programming.  Method despatch takes place
  based on the class of the first argument to the generic function.
}
\details{
  An \R ``object'' is a data object which has a \code{class} attribute.
  A class attribute is a character vector giving the names of
  the classes which the object ``inherits'' from.  When a generic
  function \code{fun} is applied to an object with class attribute
  \code{c("first", "second")}, the system searches for a function called
  \code{fun.first} and, if it finds it, applies it to the object.  If no
  such function is found, a function called \code{fun.second} is tried.
  If no class name produces a suitable function, the function
  \code{fun.default} is used.

  The function \code{class} prints the vector of names of classes an
  object inherits from.  Correspondingly, \code{class<-} sets the
  classes an object inherits from.

  \code{unclass} returns (a copy of) its argument with its class
  information removed.

  \code{inherits} indicates whether its first argument inherits from any
  of the classes specified in the \code{what} argument. If \code{which}
  is \code{TRUE} then an integer vector of the same length as
  \code{what} is returned. Each element indicates the position in the
  \code{class(x)} matched by the element of \code{what}; zero indicates
  no match. If \code{which} is \code{FALSE} then \code{TRUE} is 
  returned by \code{inherits} if any of the names in \code{what} match
  with any \code{class}.
}
\seealso{
  \code{\link{UseMethod}}, \code{\link{NextMethod}}.
}
\examples{
  x <- 10
  inherits(x, "a") #FALSE
  class(x)<-c("a", "b")
  inherits(x,"a") #TRUE
  inherits(x, "a", TRUE) # 1
  inherits(x, c("a", "b", "c"), TRUE) # 1 2 0
}
\keyword{methods}
\keyword{classes}
