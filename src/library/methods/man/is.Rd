\name{is}
\alias{is}
\alias{extends}
\alias{setIs}
\title{Is an Object from a Class}
\description{
  \code{is}: 
  With two arguments, tests whether \code{object} can be treated as from
  \code{class2}.

  With one argument, returns all the super-classes of this object's class.

  \code{extends}: 
  Does the first class extend the second class?
  Returns \code{maybe} if the extension includes a test.

  \code{setIs}: 
  Defines \code{class1} to be an extension of \code{class2}.
}
\usage{
is(object, class2)

extends(class1, class2, maybe=TRUE)

setIs(class1, class2, test=NULL, coerce=NULL, replace=NULL,
      by = NULL, where = 1)

}
\arguments{
  \item{object}{Any R object.}
  \item{class1, class2}{
    The names of the classes between which \code{is} relations are to be
    defined.}
  \item{maybe}{What value to return if the relationship is conditional.}
  \item{test, coerce, replace}{
    Functions optionally supplied to test whether the relation is
    defined, to coerce the object to \code{class2}, and to alter the
    object so that \code{is(object, class2)} is identical to
    \code{value}.}
  \item{by}{
    The name of an intermediary class.  Coercion will proceed by first
    coercing to this class and from there to the target class.  (The
    intermediate coercions have to be valid.)}
  \item{where}{
    Where to store the metadata defining the relationship. Default is
    the global environment.}
}

\details{
  
  \code{setIs}: 
  
The relationship can be conditional, if a function is supplied as the \code{test}
argument.  If a function is supplied as the \code{coerce} argument, this function will
be applied to any \code{class1} object in order to turn it into a
\code{class2} object.
If the relationship is to be defined indirectly through a third class,
this class can be named in the \code{by} argument.

Extension may imply that a \code{class1} object contains a \code{class2} object.  The default
sense of containing is that all the slots of the simpler class are found in the
more elaborate one.  If the \code{replace} argument is supplied as an S replacement
function, this function will be used to implement \code{as(obj, class2)
  <- value}.

The \code{coerce}, \code{replace}, and \code{by} arguments behave
as described for the \code{\link{setAs}} function. It's  unlikely you
would use the \code{by} argument directly, but it is used in defining
cached information about classes. The value returned (invisibly) by
\code{setIs} is the extension information, as a list.

Information about \code{setIs} relations can be stored in the metadata
for either \code{class1} (in the \code{extends} information) or in the
metadata for \code{class2} (in the \code{subclasses} information).  For
the information to be retained for a future session, one of these
classes must be defined in the global
environment, since only objects assigned there are saved by
\code{\link{save.image}}.  If neither class is defined in environment
\code{where}, \code{setIs} generates an error.

Because only global environment information is saved, it rarely makes
sense to give a value other than the default for argument \code{where}.
One exception is \code{where=0}, which modifies the cached (i.e.,
session-scope) information about the class.  Class completion
computations use this version, but don't use it yourself unless you are
quite sure you know what you're doing.

}
\references{
  The R package \code{methods} implements, with a few exceptions, the
  programming interface for classes
  and methods in the book \emph{Programming with Data} (John
  M. Chambers, Springer, 1998), in particular sections 1.6, 2.7, 2.8,
  and chapters 7 and 8.

  While the programming interface for the methods package follows the reference,
  the R software is an original implementation, so details in
  the reference that reflect the S4 implementation may appear
  differently in R.  Also, there are extensions to the programming
  interface developed more recently than the reference.  For a
  discussion of details and ongoing development, see the web page 
  \url{http://developer.r-project.org/methodsPackage.html} and the
  pointers from that page.
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
