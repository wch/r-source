% File src/library/methods/man/setClass.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Development Team
% Distributed under GPL 2 or later

\name{setClass}
\alias{setClass}
\title{Create a Class Definition}
\description{
  Create  a class definition, specifying the representation (the slots) and/or the classes contained in this one (the superclasses), plus other optional details.
}
\usage{
setClass(Class, representation, prototype, contains=character(),
         validity, access, where, version, sealed, package)
}
\arguments{
  \item{Class}{character string name for the class.}
  \item{representation}{ a named list of the slots that the new class should have, the names giving the names of the slots and the corresponding elements being the character string names of  the corresponding classes.
     Usually a call to
    the \code{\link{representation}} function. 

Backward compatibility and compatibility with S-Plus allows unnamed
elements for superclasses, but the recommended style is to use the
\code{contains=} argument instead.}
  \item{prototype}{ an object providing the default
    data for the slots in this class.  Usually and preferably the
    result of a call to \code{\llink{prototype}}. }
  \item{contains}{ what classes does this class extend?  (These are
    called \emph{superclasses} in some languages.)  When these classes
    have slots, all their slots will be contained in the new class as
    well. }
  \item{where}{ the
    environment in which to store or remove the
    definition.  Defaults to the top-level environment of the calling function
    (the global environment for ordinary computations, and the
    environment or name space of a package in the source code for that package).
  }
  \item{validity}{ if supplied, should be a validity-checking method
    for objects from this class (a function that returns \code{TRUE} if
    its argument is a valid object of this class and one or more strings
    describing the failures otherwise).  See \code{\link{validObject}}
    for details.}
  \item{access, version}{access and version, included for
      compatibility with S-Plus, but currently ignored.}
  \item{sealed}{ if \code{TRUE}, the class definition will be sealed,
    so that another call to \code{setClass} will fail on this class name.
  }
  \item{package}{ an optional package name for the class.  By default
    (and usually) the name of the package in which the class definition is assigned.
  }
}
\section{Basic Use: Slots and Inheritance}{
The two essential arguments, other than the class name are
\code{representation} and \code{contains}, defining the explicit slots
and the inheritance (superclasses). Together, these arguments define
all the information in an object from this class; that is, the names
of all the slots and the classes required for each of them.

The name of the class determines
which methods apply directly to objects from this class.  The 
inheritance information specifies which methods apply indirectly,
through inheritance.  See \link{Methods}.

The slots in a class definition will be the union of all the slots
specified directly by \code{representation} and all the slots in all
the contained classes.
There can only be one slot with a given name; specifically, the
direct and inherited slot names must be unique.
That does not, however, prevent the same class from being inherited
via more than one path.

One kind of element in the \code{contains=} argument is special, specifying one of the R
vector types or one of a few other special \R types (\code{matrix} and
\code{array}).
A class can contain at most one of these, directly or indirectly.
When it does, that contained class determines the \dQuote{data part}
of the class, and objects from the new class inherit all the built in
behavior of the contained type.
Two direct consequences are:  for any object \code{x} from the class,
\code{typeof(x)} will be the contained basic type; and a special
pseudo-slot, \code{.Data}, will be shown with the corresponding class.

For an object from any class that does \emph{not} contain such a type,
\code{typeof(x)} will be \code{"S4"} and the class will have no
\code{.Data} slot.
}

\note{
  Certain slot names are not allowed in the current implementation, as
  they correspond to \link{attributes} which are treated
  specially.  These are \code{class}, \code{comment}, \code{dim},
  \code{dimnames}, \code{names}, \code{row.names} and \code{tsp}.
}
\section{ Prototypes}{
  Defining new classes that inherit from (\sQuote{extend}) other classes
  is a powerful technique, but has to be used carefully and not
  over-used.  Otherwise, you will often get unintended results when you
  start to compute with objects from the new class.

  As shown in the examples below, the simplest and safest form of
  inheritance is to start with an explicit class, with some slots, that
  does not extend anything else.  It only does what we say it does.

  Then extensions will add some new slots and new behavior.

  Another variety of extension starts with one of the built-in objects,
  perhaps with the intention of modifying R's standard behavior for that
  class.  In this case, the new class inherits the built-in data type
  as its \sQuote{data} part.  See the \code{"numWithId"} example below.

  When such a class definition is printed, the data part shows up as a
  pseudo-slot named \code{.Data}.
}


\section{Classes and Packages}{

Class definitions normally belong to packages (but can be defined in
the  global environment as well, by evaluating the expression on the
command line or in a file sourced from the command line).
The corresponding package name is part of the class definition; that
is, part of the \code{classRepresentation} object holding that
definition.  Thus, two classes with the same name can exist in
different packages, for most purposes.

When a class name is supplied for a slot or a superclass, a
corresponding class definition will be found, looking from the
namespace or environment of the current package, assuming the call to
\code{setClass} in question appears directly in the source for the
package.  That's where it should appear, to avoid ambiguity.

In particular, if the current package has a namespace then the  class
must be found in the current package itself, in the imports defined by that namespace, or in the base
package.

When this rule does not identify a class uniquely (because it appears
in more than one imported package) then the \code{\link{packageSlot}}
of the character string name needs to be supplied with the name.
This should be a rare occurrence.
}

\references{
 Chambers, John M. (2008)
 \emph{Software for Data Analysis: Programming with R}
  Springer.  (For the R version.)

 Chambers, John M. (1998)
 \emph{Programming with Data}
 Springer (For the original S4 version.) 
}
\seealso{
\code{\link{Classes}} for a general discussion of classes,
  \code{\link{Methods}} for an analogous discussion of methods,
  \code{\link{makeClassRepresentation}}
}
\examples{
\dontshow{
 if(isClass("trackMultiCurve")) removeClass("trackMultiCurve")
 if(isClass("trackCurve"))	removeClass("trackCurve")
 if(isClass("track"))		removeClass("track")
}
## A simple class with two slots
setClass("track",
         representation(x="numeric", y="numeric"))
## A class extending the previous, adding one more slot
setClass("trackCurve",
    representation(smooth = "numeric"),
    contains = "track")
## A class similar to "trackCurve", but with different structure
## allowing matrices for the "y" and "smooth" slots
setClass("trackMultiCurve",
         representation(x="numeric", y="matrix", smooth="matrix"),
         prototype = list(x=numeric(), y=matrix(0,0,0),
                          smooth= matrix(0,0,0)))
##
## Suppose we want trackMultiCurve to be like trackCurve when there's
## only one column.
## First, the wrong way.
try(setIs("trackMultiCurve", "trackCurve",
    test = function(obj) {ncol(slot(obj, "y")) == 1}))

## Why didn't that work?  You can only override the slots "x", "y",
## and "smooth" if you provide an explicit coerce function to correct
## any inconsistencies:

setIs("trackMultiCurve", "trackCurve",
  test = function(obj) {ncol(slot(obj, "y")) == 1},
  coerce = function(obj) {
     new("trackCurve",
         x = slot(obj, "x"),
         y = as.numeric(slot(obj,"y")),
         smooth = as.numeric(slot(obj, "smooth")))
  })

## A class that extends the built-in data type "numeric"

setClass("numWithId", representation(id = "character"),
         contains = "numeric")

new("numWithId", 1:3, id = "An Example")

\dontshow{
tMC <- new("trackMultiCurve")
is.matrix(slot(tMC, "y"))
is.matrix(slot(tMC, "smooth"))
setClass("myMatrix", "matrix", prototype = matrix(0,0,0))
nrow(new("myMatrix")) # 0
nrow(new("matrix")) # 1
## simple test of prototype data
xxx <- stats::rnorm(3)
setClass("xNum", representation(x = "numeric"), prototype = list(x = xxx))
stopifnot(identical(new("xNum")@x, xxx))
### tests of the C macros MAKE_CLASS and NEW
### FIXME:  there should be a separate man page for the C-level macros
### and the tests below should be there.
stopifnot(identical(.Call("R_methods_test_MAKE_CLASS", "trackCurve",
                          PACKAGE = "methods"),
                    getClass("trackCurve")))

stopifnot(identical(.Call("R_methods_test_NEW", "track", PACKAGE = "methods"),
                    new("track")))


## The following should not be needed.  But make check removes all files
## between example files, in a crude way that does not cause the class
## information to be reset.  There seems no way to detect this, so we
## have to remove classes ourselves

removeClass("withId")
removeClass("maybeNumber")
removeClass("xNum")
removeClass("myMatrix")
resetClass("integer")
resetClass("numeric")
resetClass("logical")
removeClass("trackMultiCurve")
removeClass("trackCurve")
removeClass("track")
}%dont show
}
\keyword{programming}
\keyword{classes}
\keyword{methods}
