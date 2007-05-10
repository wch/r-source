\name{setClass}
\alias{setClass}
\alias{removeClass}
\alias{resetClass}
\alias{isClass}
\alias{getClasses}
\alias{findClass}
\alias{sealClass}
\title{Create a Class Definition}
\description{
  Functions to create (\code{setClass}) and manipulate class definitions.
}
\usage{
setClass(Class, representation, prototype, contains=character(),
         validity, access, where, version, sealed, package)

removeClass(Class, where)

isClass(Class, formal=TRUE, where)

getClasses(where, inherits = missing(where))

findClass(Class, where, unique = "")

resetClass(Class, classDef, where)

sealClass(Class, where)

}
\arguments{
  \item{Class}{character string name for the class.  Other than
    \code{setClass}, the functions will usually take a class definition
    instead of the string (allowing the caller to identify the class uniquely).}
  \item{representation}{ the slots that the new class should have
    and/or other classes that this class extends.  Usually a call to
    the \code{\link{representation}} function. }
  \item{prototype}{ an object (usually a list) providing the default
    data for the slots specified in the representation. }
  \item{contains}{ what classes does this class extend?  (These are
    called \emph{superclasses} in some languages.)  When these classes
    have slots, all their slots will be contained in the new class as
    well. }
  \item{where}{ For \code{setClass} and \code{removeClass}, the
    environment in which to store or remove the
    definition.  Defaults to the top-level environment of the calling function
    (the global environment for ordinary computations, but the
    environment or name space of a package when loading that package).

    For other functions, \code{where} defines where to do the search
    for the class definition, and the default is to search from the top-level
    environment or name space of the caller to this function.
  }
  \item{unique}{if \code{findClass} expects a unique location for the
    class, \code{unique} is a character string explaining the purpose
    of the search (and is used in warning and error messages).  By
    default, multiple locations are possible and the function always
    returns a list.
  }
  \item{inherits}{in a call to \code{getClasses}, should the value
    returned include all parent environments of \code{where}, or that
    environment only?  Defaults to \code{TRUE} if \code{where} is
    omitted, and to \code{FALSE} otherwise.
  }
  \item{validity}{ if supplied, should be a validity-checking method
    for objects from this class (a function that returns \code{TRUE} if
    its argument is a valid object of this class and one or more strings
    describing the failures otherwise).  See \code{\link{validObject}}
    for details.}
  \item{access}{Access list for the class.  Saved in the definition, but
    not currently used.}
  \item{version}{A version indicator for this definition. Saved in the
    definition, but not currently used.}
  \item{sealed}{ If \code{TRUE}, the class definition will be sealed,
    so that another call to \code{setClass} will fail on this class name.
  }
  \item{package}{ An optional package name for the class.  By default
    (and usually) the package where the class definition is assigned
    will be used.
  }
  \item{formal}{ Should a formal definition be required? }
  \item{classDef}{ For \code{removeClass}, the optional class
    definition (but usually it's better for \code{Class} to be the
    class definition, and to omit \code{classDef}).
  }
}

\details{
  These are the functions that create and manipulate formal class
  definitions.  Brief documentation is provided below.  See the
  references for an introduction and for more details.

  \describe{
    \item{\code{setClass}:}{
      Define \code{Class} to be an S-style class.  The effect is to
      create an object, of class \code{"classRepEnvironment"}, and store
      this (hidden) in the specified environment or database.  Objects
      can be created from the class (e.g., by calling
      \code{\link{new}}), manipulated (e.g., by accessing the object's
      slots), and methods may be defined including the class name in the
      signature (see \code{\link{setMethod}}).
    }
    \item{\code{removeClass}:}{
      Remove the definition of this class, from the environment
      \code{where} if this argument is supplied; if not,
      \code{removeClass} will search for a definition, starting in the
      top-level environment of the call to \code{removeClass}, and
      remove the (first) definition found.
    }
    \item{\code{isClass}:}{
      Is this the name of a formally defined class? (Argument
      \code{formal} is for compatibility and is ignored.)
    }
    \item{\code{getClasses}:}{
      The names of all the classes formally defined on \code{where}.  If
      called with no argument, all the classes visible from the
      calling function (if called from the top-level, all the classes
      in any of the environments on the search list).  The
      \code{inherits} argument can be used to search a particular
      environment and all its parents, but usually the default setting
      is what you want.
    }
    \item{\code{findClass}:}{
      The list of environments or positions on the search list in
      which a class definition of \code{Class} is found.  If
      \code{where} is supplied, this is an environment (or name space)
      from which the search takes place; otherwise the top-level
      environment of the caller is used.  If \code{unique} is supplied
      as a character string, \code{findClass} returns a single
      environment or position.  By default, it always returns a list.
      The calling function should select, say, the first element as a
      position or environment for functions such as \code{\link{get}}.

      If \code{unique} is supplied as a character string,
      \code{findClass} will warn if there is more than one definition
      visible (using the string to identify the purpose of the call),
      and will generate an error if no definition can be found.
    }
    \item{\code{resetClass}:}{
      Reset the internal definition of a class.  Causes the complete
      definition of the class to be re-computed, from the
      representation and superclasses specified in the original
      call to \code{setClass}.

      This function is called when aspects of the class definition are
      changed.  You would need to call it explicitly if you changed the
      definition of a class that this class extends (but doing that in
      the  middle of a session is living dangerously, since it may
      invalidate existing objects).
    }
    \item{\code{sealClass}:}{ Seal the current definition of the specified
      class, to prevent further changes.  It is possible to seal a class
      in the call to \code{setClass}, but sometimes further changes have
      to be made (e.g., by calls to \code{setIs}).  If so, call
      \code{sealClass} after all the relevant changes have been made.
    }
  }
}
\section{Inheritance and Prototypes}{
  Defining new classes that inherit from (\dQuote{extend}) other classes
  is a powerful technique, but has to be used carefully and not
  over-used.  Otherwise, you will often get unintended results when you
  start to compute with objects from the new class.

  As shown in the examples below, the simplest and safest form of
  inheritance is to start with an explicit class, with some slots, that
  does not extend anything else.  It only does what we say it does.

  Then extensions will add some new slots and new behavior.

  Another variety of extension starts with one of the built-in data types,
  perhaps with the intention of modifying R's standard behavior for that
  class.  In this case, the new class inherits the built-in data type
  as its \dQuote{data} part.  See the \dQuote{numWithId} example below.

  When such a class definition is printed, the data part shows up as a
  pseudo-slot named \dQuote{.Data}.
}

\section{S3 Classes}{

  Earlier, informal classes of objects (usually referred to as
  \dQuote{S3} classes) are used by many R functions.  It's natural to
  consider including them as the class for a slot in a formal class,
  or even as a class to be extended by the new class.  This isn't
  prohibited but there are some disadvantages, and if you do want to
  include S3 classes, they should be declared by including them in a
  call to \code{\link{setOldClass}}.  Here are some considerations:

  \itemize{
  \item Using S3 classes somewhat defeats the purpose of defining a
    formal class:  An important advantage to your users is that a formal
    class provides guarantees of what the object contains (minimally,
    the classes of the slots and therfore what data they contain;
    optionally, any other requirements imposed by a validity method).

    But there is no guarantee whatever about the data in an object from
    an S3 class.  It's entirely up to the functions that create or
    modify such objects.  If you want to provide guarantees to your
    users, you will need a valdity method that explicitly checks the
    contents of S3-class objects.

  \item To get the minimal guarantee (that the object in a slot has, or
    extends, the class for the slot) you should ensure that the S3
    classes are known to \emph{be} S3 classes, with the possible
    inheritance.  To do this, include a call to
    \code{\link{setOldClass}} for the S3 classes used.

    Otherwise, the S3 class is undefined (and the code used by
    \code{setClass} will issue a warning).  Slot assignments, for
    example, will not then check for possible errors.

  \item These caveats apply to S3 classes; that is, objects with a
    class assigned by some R function but without a formal class
    definition.  In contrast, the built-in data types (\code{numeric},
    \code{list}, etc.) are generally fine as slots or for
    \code{contains=} classes (see the previous section).  These data
    types don't have formal slots, but the base code in the system
    essentially forces them to contain the type of data they claim to
    have.

    The data types \code{matrix} and \code{array} are somewhat in
    between.  They do not have an explicit S3 class, but do have one or
    two attributes.  There is no general problem in having these as
    slots, but because there is no guarantee of a dimnames slot, they
    don't work as formal classes.  The \code{ts} class is treated as a
    formal class, extending class \code{vector}.
  }

}% S3 classes

\note{
  Certain slot names are not allowed in the current implementation, as
  they correspond to \link{attributes} which are treated
  specially.  These are \code{class}, \code{comment}, \code{dim},
  \code{dimnames}, \code{names}, (from \R 2.4.0) \code{row.names}
  and \code{tsp}.
}

\references{
  The \R package \pkg{methods} implements, with a few exceptions, the
  programming interface for classes and methods in the book
  \emph{Programming with Data} (John M. Chambers, Springer, 1998), in
  particular sections 1.6, 2.7, 2.8, and chapters 7 and 8.

  While the programming interface for the \pkg{methods} package follows
  the reference, the \R software is an original implementation, so
  details in the reference that reflect the S4 implementation may appear
  differently in \R.  Also, there are extensions to the programming
  interface developed more recently than the reference.  For a
  discussion of details see \code{?\link{Methods}}
  and the links from that documentation.
}
\seealso{
  \code{\link{setClassUnion}},
  \code{\link{Methods}},
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
         representation("track", smooth = "numeric"))
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
