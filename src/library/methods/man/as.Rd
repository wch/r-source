\name{as}
\alias{as}
\alias{as<-}
\alias{coerce}
\alias{asFunctions}
\alias{setAs}
\title{Force an Object to Belong to a Class}
\usage{
as(object, Class, coerceFlag=TRUE)

coerce(x, y)

setAs(from, to, def, where = 1)

}
\description{

\describe{
  \item{\code{as}:}{
  Returns the version of this object coerced to be the given \code{Class}.  
  
  
If the corresponding \code{is} relation is true, it will be used.  In particular,
if the relation has a coerce method, the method will be invoked on \code{object}.

If the \code{is} relation is FALSE, and \code{coerceFlag} is \code{TRUE},
the coerce function will be called (which will throw an error if there is
no valid way to coerce the two objects).  Otherwise, \code{NULL} is
returned.

Coerce methods are pre-defined for basic classes (including all the types of
vectors, functions and a few others).  The object \code{asFunctions}
contains the list of such pre-defined relations:
\code{names(asFunctions)} gives the names of all the classes.

Beyond these two sources of  methods, further methods are defined by
calls to the \code{setAs} function.
}

  \item{\code{coerce}:}{
  Coerce \code{x} to be of the same class as \code{y}.  

  Not a function you should usually
call explicitly.  The function \code{\link{setAs}} creates
methods for \code{coerce} for the \code{as} function to use.
}

  \item{\code{setAs}:}{
  The function supplied as the third argument is to be called to
  implement \code{as(x, to)} when \code{x} has class \code{from}.
  Need we add that the function should return a suitable object with
  class \code{to}.
}
}
}
\references{
The web page \url{http://www.omegahat.org/RSMethods/index.html} is the primary documentation.

The functions in this package emulate the facility for classes and methods described in
\emph{Programming with Data}, (John M. Chambers, Springer, 1998).  See this book
for further details and examples.
}

\examples{
## using the definition of class "track" from \link{Classes}

\testonly{
setClass("track",
            representation(x="numeric", y="numeric"))
setClass("trackCurve",
            representation("track", smooth = "numeric"))

ydata <- rnorm(10); ysmooth <- 1:10}

setAs("track", "numeric", function(x, ...)x@y)

\testonly{
setClass("ca", representation(a = "character", id = "numeric"))

setClass("cb", representation(b = "character", id = "numeric"))


setClass("id")
setIs("ca", "id")
setIs("cb", "id")


setAs("id", "numeric", function(x, ...) x@id)

CA <- new("ca", a ="A", id = 1)

CB <- new("cb", b = "B", id = 2)

setAs("cb", "ca", function(object)new("ca", a=object@b, id = object@id))

as(CB, "numeric")

}

}

\author{
  John Chambers
}
\keyword{programming}
\keyword{classes}
\keyword{methods}
