\name{class}
\title{Object Classes}
\alias{class}
\alias{class<-}
\alias{oldClass}
\alias{oldClass<-}
\alias{unclass}
\alias{inherits}
\description{
  \R possesses a simple generic function mechanism which can be used for
  an object-oriented style of programming.  Method dispatch takes place
  based on the class of the first argument to the generic function.
}
\usage{
class(x)
class(x) <- value
unclass(x)
inherits(x, what, which = FALSE)

oldClass(x)
oldClass(x) <- value
}
\arguments{
  \item{x}{a \R object}
  \item{what, value}{a character vector naming classes.}
  \item{which}{logical affecting return value: see Details.}
}

\details{
  Many \R objects have a \code{class} attribute, a character vector
  giving the names of the classes which the object \dQuote{inherits}
  from.  If the object does not have a class attribute, it has an
  implicit class, \code{"matrix"}, \code{"array"} or the result of
  \code{\link{mode}(x)}.  (Functions \code{oldClass} and
  \code{oldClass<-} get and set the attribute, which can also be done
  directly.)

  When a generic
  function \code{fun} is applied to an object with class attribute
  \code{c("first", "second")}, the system searches for a function called
  \code{fun.first} and, if it finds it, applies it to the object.  If no
  such function is found, a function called \code{fun.second} is tried.
  If no class name produces a suitable function, the function
  \code{fun.default} is used (if it exists).
  If there is no class attribute, the implicit class is tried, then the
  default method.

  The function \code{class} prints the vector of names of classes an
  object inherits from.  Correspondingly, \code{class<-} sets the
  classes an object inherits from.

  \code{unclass} returns (a copy of) its argument with its class
  attribute removed.  (It is not allowed for objects which cannot be
  copied, namely environments and external pointers.)

  \code{inherits} indicates whether its first argument inherits from any
  of the classes specified in the \code{what} argument.  If \code{which}
  is \code{TRUE} then an integer vector of the same length as
  \code{what} is returned. Each element indicates the position in the
  \code{class(x)} matched by the element of \code{what}; zero indicates
  no match. If \code{which} is \code{FALSE} then \code{TRUE} is 
  returned by \code{inherits} if any of the names in \code{what} match
  with any \code{class}.
}

\note{
  Functions \code{oldClass} and \code{oldClass<-} behave in the same way
  as functions of those names in S-PLUS 5/6, \emph{but} in \R
  \code{\link{UseMethod}} dispatches on the class as returned by
  \code{class} rather than \code{oldClass}.  \emph{However},
  \link{group generic}s dispatch on the \code{oldClass} for efficiency.
}

\section{Formal classes}{
  An additional mechanism of \emph{formal} classes is available in
  packages \pkg{methods} which is attached by default.  For objects
  which have a formal class, its name is returned by \code{class}
  as a character vector of length one.

  The replacement version of the function sets the class to the value
  provided.  For classes that have a formal definition, directly
  replacing the class this way is strongly deprecated.  The expression
  \code{\link[methods]{as}(object, value)} is the way to coerce an object to a
  particular class.
}
\seealso{
  \code{\link{UseMethod}}, \code{\link{NextMethod}},
  \code{\link{group generic}}.
}
\examples{
x <- 10
inherits(x, "a") #FALSE
class(x) <- c("a", "b")
inherits(x,"a") #TRUE
inherits(x, "a", TRUE) # 1
inherits(x, c("a", "b", "c"), TRUE) # 1 2 0
}
\keyword{methods}
\keyword{classes}
