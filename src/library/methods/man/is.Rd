\name{is}
\alias{is}
\alias{extends}
\alias{setIs}
\title{Is an Object from a Class}
\usage{
is(object, class2)

extends(class1, class2, maybe=TRUE)

setIs(class1, class2, test=NULL, coerce=NULL, replace=NULL, where=-1)

}
\description{
  
  \code{is}: 
  With two arguments, tests whether \code{object} can be treated as from \code{class2}.

With one argument, returns all the super-classes of this object's class.

  
  \code{extends}: 
  Does the first class extend the second class?
Returns \code{maybe} if the extension includes a test.

  
  \code{setIs}: 
  Defines class1 to be an extension of class2.  
}
\arguments{
  \item{object}{Any R object.}
  \item{class1, class2}{
    The names of the classes between which \code{is} relations are to be defined.
  }
  \item{maybe}{What value to return if the relationship is conditional.
  }
  \item{test, coerce, replace}{
    Functions optionally supplied to test whether the relation is
    defined, to coerce the object to \code{class2}, and to alter the
    object so that \code{is(object, class2)} is identical to \code{value}.
  }
  \item{where}{
    Where to store the metadata defining the relationship. Default is
    the global environment.
  }
  
}

\details{
  
  \code{setIs}: 
  
The relationship can be conditional, if a function is supplied as the \code{test}
argument.  If a function is supplied as the \code{coerce} argument, this function will
be applied to any \code{class1} object in order to turn it into a \code{class2} object.

Extension may imply that a \code{class1} object contains a \code{class2} object.  The default
sense of containing is that all the slots of the simpler class are found in the
more elaborate one.  If the \code{replace} argument is supplied as an S replacement
function, this function will be used to implement \code{as(obj, class2) <- value}.

}
\references{
The web page \url{http://www.omegahat.org/RSMethods/index.html} is the primary documentation.

The functions in this package emulate the facility for classes and methods described in
\emph{Programming with Data}, (John M. Chambers, Springer, 1998).  See this book
for further details and examples.
}
\author{
  John Chambers
}
\examples{
\testonly{
if(isClass("track"))
  removeClass("track")
if(isClass("trackCurve"))
  removeClass("trackCurve")
if(isClass("trackMultiCurve"))
  removeClass("trackMultiCurve")
## A simple class with two slots
setClass("track",
            representation(x="numeric", y="numeric"))
## A class extending the previous, adding one more slot
}
## a class definition (see \link{setClass} for the example)
setClass("trackCurve",
            representation("track", smooth = "numeric"))
## A class similar to "trackCurve", but with different structure
## allowing matrices for the "y" and "smooth" slots
setClass("trackMultiCurve", representation(x="numeric", y="matrix", smooth="matrix"),
          prototype = structure(list(), x=numeric(), y=matrix(0,0,0), smooth= matrix(0,0,0)))
## Define a multi-curve to extend a single curve ONLY
## if the y data is one variable.
setIs("trackMultiCurve", "trackCurve", test = function(obj) {ncol(slot(obj, "y")) == 1},
      coerce = function(obj) { new("trackCurve", x = slot(obj, "x"),
        y = as.numeric(slot(obj,"y")), curve = as.numeric(slot(obj, "curve")))})

}
\keyword{programming}
\keyword{classes}
\keyword{methods}
