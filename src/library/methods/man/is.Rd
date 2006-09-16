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

extends(class1, class2, maybe=TRUE, fullInfo = FALSE)

setIs(class1, class2, test=NULL, coerce=NULL, replace=NULL,
      by = character(), where = topenv(parent.frame()), classDef =,
      extensionObject = NULL, doComplete = TRUE)
}
\arguments{
  \item{object}{any \R object.}
  \item{class1, class2}{
    the names of the classes between which \code{is} relations are to be
    defined.}

  \item{maybe, fullInfo}{
      In a call to \code{extends}, \code{maybe} is a flag to
      include/exclude conditional relations, and \code{fullInfo} is a
      flag, which if \code{TRUE} causes object(s) of class
      \code{classExtension} to be returned, rather than just the
      names of the classes or a logical value.  See the details
      below.}

  \item{extensionObject}{ alternative to the \code{test, coerce,
    replace, by} arguments; an object from class
    \code{SClassExtension} describing the relation.  (Used in internal calls.)
    }

  \item{doComplete}{when \code{TRUE}, the class definitions will be
  augmented with indirect relations as well.  (Used in internal calls.)}


  \item{test, coerce, replace}{
    In a call to \code{setIs}, functions optionally supplied to test whether the relation is
    defined, to coerce the object to \code{class2}, and to alter the
    object so that \code{is(object, class2)} is identical to
    \code{value}.}
  \item{by}{
    In a call to \code{setIs}, the name of an intermediary class.  Coercion will proceed by first
    coercing to this class and from there to the target class.  (The
    intermediate coercions have to be valid.)}
  \item{where}{
    In a call to \code{setIs}, where to store the metadata defining the relationship. Default is
    the global environment.}
  \item{classDef}{
    Optional class definition for \code{class} , required internally
    when \code{setIs} is called during the initial definition of the
    class by a call to \code{\link{setClass}}. \emph{Don't} use this
    argument, unless you really know why you're doing so.}
}

\details{
\describe{
  \item{\code{extends}:}{

  Given two class names, \code{extends} by default says whether the
  first class extends the second; that is, it does for class names
  what \code{is} does for an object and a class.  Given one class
  name, it returns all the classes that class extends (the
  \dQuote{superclasses} of that class), including the class itself.  If
  the flag \code{fullInfo} is \code{TRUE}, the result is a list, each
  element of which is an object describing the relationship;
  otherwise, and by default, the value returned is only the names of
  the classes.}

  \item{\code{setIs}: }{

    This function establishes an inheritance relation between two
    classes, by some means other than having one class contain the
    other.  It should \emph{not} be used for ordinary relationships:
    either include the second class in the \code{contains=} argument
    to \code{\link{setClass}} if the class is contained in the usual
    way, or consider \code{setClassUnion} to define a virtual class
    that is extended by several ordinary classes.  A call to
    \code{setIs} makes sense, for example, if one class ought to be
    automatically convertible into a second class, but they have
    different representations, so that the conversion must be done by
    an explicit computation, not just be inheriting slots, for
    example.  In this case, you will typically need to provide both a
    \code{coerce=} and \code{replace=} argument to \code{setIs}.


  The \code{coerce}, \code{replace}, and \code{by} arguments behave as
  described for the \code{\link{setAs}} function.  It's unlikely you
  would use the \code{by} argument directly, but it is used in defining
  cached information about classes.  The value returned (invisibly) by
  \code{setIs} is the extension information, as a list.

  The  \code{coerce} argument is a function that turns a
  \code{class1} object into a \code{class2} object.  The
  \code{replace} argument is a function of two arguments that modifies a \code{class1}
  object (the first argument) to replace the part of it that
  corresponds to \code{class2} (supplied as \code{value}, the second
  argument).  It then returns the modified object as the value of the
  call.  In other words, it acts as a replacement method to
  implement the expression \code{as(object, class2) <- value}.

  The easiest way to think of the  \code{coerce} and \code{replace}
  functions is by thinking of the case that  \code{class1}
  contains \code{class2} in the usual sense, by including the slots of
  the second class.  (To repeat, in this situation you would not call
  \code{setIs}, but the analogy shows what happens when you do.)

  The \code{coerce} function in this case would just make a
  \code{class2} object by extracting the corresponding slots from the
  \code{class1} object. The \code{replace} function would replace in
  the \code{class1} object the slots corresponding to \code{class2},
  and return the modified object as its value.


  The relationship can also be conditional, if a function is supplied as the
  \code{test} argument.  This should be a function of one argument
  that returns \code{TRUE} or \code{FALSE} according to whether the
  object supplied satisfies the relation \code{is(object, class2)}.
  If you worry about such things, conditional relations between
  classes are slightly deprecated because they cannot be implemented
  as efficiently as ordinary relations and because they sometimes can
  lead to confusion (in thinking about what methods are dispatched for
  a particular function, for example).  But they can correspond to
  useful distinctions, such as when two classes have the same representation,
  but only one of them obeys certain additional constraints.

  Because only global environment information is saved, it rarely makes
  sense to give a value other than the default for argument
  \code{where}.  One exception is \code{where=0}, which modifies the
  cached (i.e., session-scope) information about the class.  Class
  completion computations use this version, but don't use it yourself
  unless you are quite sure you know what you're doing.
}
}
}
\references{
  The R package \pkg{methods} implements, with a few exceptions, the
  programming interface for classes and methods in the book
  \emph{Programming with Data} (John M. Chambers, Springer, 1998), in
  particular sections 1.6, 2.7, 2.8, and chapters 7 and 8.

  While the programming interface for the \pkg{methods} package follows
  the reference, the R software is an original implementation, so
  details in the reference that reflect the S4 implementation may appear
  differently in R.  Also, there are extensions to the programming
  interface developed more recently than the reference.  For a
  discussion of details see \code{?\link{Methods}}
  and the links from that documentation.
}
\examples{
\dontshow{
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
setClass("trackMultiCurve",
         representation(x="numeric", y="matrix", smooth="matrix"),
         prototype = structure(list(), x=numeric(), y=matrix(0,0,0),

                               smooth= matrix(0,0,0)))
## Automatically convert an object from class "trackCurve" into
## "trackMultiCurve", by making the y, smooth slots into 1-column matrices
setIs("trackCurve",
      "trackMultiCurve",
      coerce = function(obj) {
        new("trackMultiCurve",
            x = obj@x,
            y = as.matrix(obj@y),
            curve = as.matrix(obj@smooth))
      },
      replace = function(obj, value) {
        obj@y <- as.matrix(value@y)
        obj@x <- value@x
        obj@smooth <- as.matrix(value@smooth)
        obj})

## Automatically convert the other way, but ONLY
## if the y data is one variable.
setIs("trackMultiCurve",
      "trackCurve",
      test = function(obj) {ncol(obj@y) == 1},
      coerce = function(obj) {
        new("trackCurve",
            x = slot(obj, "x"),
            y = as.numeric(obj@y),
            smooth = as.numeric(obj@smooth))
      },
      replace = function(obj, value) {
        obj@y <- matrix(value@y, ncol=1)
        obj@x <- value@x
        obj@smooth <- value@smooth
        obj})
\dontshow{
removeClass("trackMultiCurve")
removeClass("trackCurve")
removeClass("track")
}
}
\keyword{programming}
\keyword{classes}
\keyword{methods}
