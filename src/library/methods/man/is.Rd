% File src/library/methods/man/is.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Development Team
% Distributed under GPL 2 or later

\name{is}
\alias{is}
\alias{extends}
\alias{setIs}
\title{Is an Object from a Class}
\description{
Functions to test inheritance relationships between an object and a
class (\code{is}) or between two classes (\code{extends}), and to
establish such relationships (\code{setIs},  an explicit alternative to the \code{contains=} argument to \code{\link{setClass}}).
}
\section{Summary of Functions}{
  \describe{
  \item{\code{is}:}{
      With two arguments, tests whether \code{object} can be treated as from
      \code{class2}.

      With one argument, returns all the super-classes of this object's class.
    }
  \item{\code{extends}:}{
      Does the first class extend the second class?
      The call returns \code{maybe} if the extension includes a test.  

When called with one argument, the value is a vector of the
superclasses of \code{class1}.  If argument \code{fullInfo}  is
\code{TRUE}, the call returns a named list of objects of class \code{\linkS4class{SClassExtension}}; otherwise, just the names of the superclasses.
    }
  \item{\code{setIs}:}{
      Defines \code{class1} to be an extension (subclass) of \code{class2}, with \code{coerce} and \code{replace} defining methods to coerce to the superclass and to replace the part corresponding to the superclass.
    }
  }
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
    examined defined, or (more efficiently) the class definition
    objects for the classes.}

  \item{maybe, fullInfo}{
      In a call to \code{extends}, \code{maybe} is the value returned if a
      relation is conditional. In a call with \code{class2} missing, \code{fullInfo} is a
      flag, which if \code{TRUE} causes a list of objects of class
      \code{classExtension} to be returned, rather than just the
      names of the classes.
      }

  \item{test, coerce, replace}{
    In a call to \code{setIs}, functions optionally supplied to test whether the relation is
    defined, to coerce the object to \code{class2}, and to alter the
    object so that \code{is(object, class2)} is identical to
    \code{value}. See the details section below.

  The remaining arguments are for internal use and/or usually omitted.}

  \item{extensionObject}{ alternative to the \code{test, coerce,
    replace, by} arguments; an object from class
    \code{SClassExtension} describing the relation.  (Used in internal calls.)
    }

  \item{doComplete}{when \code{TRUE}, the class definitions will be
  augmented with indirect relations as well.  (Used in internal calls.)}

  \item{by}{
    In a call to \code{setIs}, the name of an intermediary class.  Coercion will proceed by first
    coercing to this class and from there to the target class.  (The
    intermediate coercions have to be valid.)}
  \item{where}{
    In a call to \code{setIs}, where to store the metadata defining the relationship. Default is
    the global environment for calls from the top level of the session or a source file evaluated there.  When the call occurs in the top level of a file in the source of a package, the default will be the namespace or environment of the package.  Other uses are tricky and not usually a good idea, unless you really know what you are doing.}
  \item{classDef}{
    Optional class definition for \code{class} , required internally
    when \code{setIs} is called during the initial definition of the
    class by a call to \code{\link{setClass}}. \emph{Don't} use this
    argument, unless you really know why you're doing so.}
}

\section{Details}{

    The function \code{setIs} establishes an inheritance relation between two
    classes, by some means other than having one class contain the
    other.  It should \emph{not} be used for ordinary relationships:
    either include the second class in the \code{contains=} argument
    to \code{\link{setClass}} if the class is contained in the usual
    way, or consider \code{\link{setClassUnion}} to define a virtual class
    that is extended by several ordinary classes.  

    A call to
    \code{setIs} does make sense, for example, if one class ought to be
    automatically convertible into a second class, but they have
    different representations, so that the conversion must be done by
    an explicit computation, not just by inheriting slots.
    In this case, you will typically need to provide both a
    \code{coerce=} and \code{replace=} argument to \code{setIs}.

    Another reasonable use of \code{setIs} may be to relate two
    classes with  some common slots, but in which one class (the
    subclass) puts extra
    constraints on the values in some slots.  In this case, simple
    \code{contains=} relations will generate methods for coercing to
    the class with the constraints that can produce invalid objects.


  The \code{coerce} and \code{replace} arguments are functions the
  define how to coerce a \code{class1} object to \code{class2}, and
  how to replace the part of the subclass object that corresponds to
  \code{class2}.  The first of these is a function of one argument
  (conventionally \code{from}) and the second of two arguments
  (\code{from}, \code{value}).  For details, see the section on coerce
  functions below .

   When \code{by} is specified, the coerce process first coerces to
   this class and then to \code{class2}.  It's unlikely you
  would use the \code{by} argument directly, but it is used in defining
  cached information about classes.

  The value returned (invisibly) by
  \code{setIs} is the revised class definition of \code{class1}.

}

\section{Coerce, replace, and test functions}{

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

  For additional discussion of these functions, see
  the documentation of the
   \code{\link{setAs}} function.  (Unfortunately, argument
   \code{def} to that function corresponds to argument \code{coerce} here.) 

  The inheritance relationship can also be conditional, if a function is supplied as the
  \code{test} argument.  This should be a function of one argument
  that returns \code{TRUE} or \code{FALSE} according to whether the
  object supplied satisfies the relation \code{is(object, class2)}.
  Conditional relations between
  classes are slightly deprecated because they cannot be implemented
  as efficiently as ordinary relations and because they sometimes can
  lead to confusion (in thinking about what methods are dispatched for
  a particular function, for example).  But they can correspond to
  distinctions such as two classes that have the same representation,
  but with only one of them obeying certain additional constraints.
}
\references{
 Chambers, John M. (2008)
 \emph{Software for Data Analysis: Programming with R}
  Springer.  (For the R version.)

 Chambers, John M. (1998)
 \emph{Programming with Data}
 Springer (For the original S4 version.)
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
