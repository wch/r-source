\name{setClass}
\alias{setClass}
\alias{removeClass}
\alias{resetClass}
\alias{isClass}
\alias{getClasses}
\title{Create a Class Definition}
\description{
Create a formally defined class with specified slots and/or
relationships to other classes.  Also functions to remove a class
definition, to test whether a
class has been defined, to test whether an object is a class definition,
and to reset the internal definition of a class.}
\usage{
setClass(Class, representation, prototype,
         contains=character(), validity, access, where=1, version=FALSE)

removeClass(Class, where=-1)

isClass(Class, formal=TRUE)
isClassDef(object)

getClasses(where)

resetClass(Class)

}
\arguments{
  \item{Class}{ character string name for the class }
  \item{representation}{ the slots that the new class should have
      and/or other classes that this class extends.  Usually a call to
      the \code{\link{representation}} function. }
  \item{prototype}{ an object (usually a list) providing the default
    data for the slots specified in the representation. }
  \item{contains}{ what classes does this class extend?  (These are
      called \emph{superclasses} in some languages.)  When these
      classes have slots, all their slots will be contained in the new
      class as well. }
  \item{where}{ What environment to use to store or remove the definition (as metadata).
    By default, uses the global environment for \code{setClass} and
    searches for a definition to remove, for \code{removeClass}. }
  \item{validity, access, version}{ Control arguments included for
      compatibility with the S-Plus API, but not currently used.  }

    \item{x}{an arbitrary object.}

    \item{formal}{
      Should a formal definition be required?
    }

    \item{object}{
      any R object.
    }
    
}

\details{

  These are the functions that create and manipulate formal class
  definitions.  Brief documentation is provided below.  See the
  references for an introduction and for more details.
 
  \describe{ 
    \item{\code{setClass}:}{ 
      Define Class to be an S-style class.  The effect is to create an
      object, of class \code{"classRepEnvironment"}, and store this
      (hidden) in the specified environment or database.  Objects can be
      created from the class (e.g., by calling \code{\link{new}}),
      manipulated (e.g., by accessing the object's slots), and methods
      may be defined including the class name in the signature (see
      \code{\link{setMethod}}).
    }

    \item{\code{removeClass}:}{ 
      Remove the definition of this class.  Calling this \emph{always}
      resets the version of the class cached for the session.  If
      \code{where=0}, that's all it does.  Otherwise, it removes the
      version from the specified environment or database (from the global
      environment by default).
    }
  
    \item{\code{isClass}:}{ 
      Is this a the name of a formally defined class? (Argument \code{formal} is for
      compatibility and is ignored.)
    }

    \item{\code{isClassDef}:}{ 
      Is this object a class definition (it will be, for example, if it
      is the value of a call to \code{\link{getClass}}, the complete
      definition of a class with its extensions, or to \code{\link{getClassDef}}, the local
      definition  of the class).
    }

    \item{\code{getClasses}:}{
      The names of all the classes formally defined on `where'.
      If called with no argument, all the classes currently known in the session
      (which does not include classes that may be defined on one of the attached
      libraries, but have not yet been used in the session).
    }

    \item{\code{unclass}:}{
      Returns the object containing the values of all the slots in this object's
      class definition (specifically, ithe returned object has attributes corresponding
      to each slot), in the case that the object's class is formally defined with slots.  
      
      For classes that extend a single other class (e.g., a basic class
      such as \code{"numeric"}) the result is an object of that class.
    }
    \item{\code{resetClass}:}{
      Reset the internal definition of a class.  The effect is that
      the next time the definition of this class is needed, it will be
      recomputed from the information on the attached databases.

      This function is called when aspects of the class definition are
      changed.  You would need to call it explicitly if you changed the
      definition of a class that this class extends (but doing that in
      the  middle of a session is living dangerously, since it may
      invalidate existing objects).
      }
  }
}
\section{Inheritance and Prototypes}{
  Defining new classes that inherit from (``extend'') other classes is a
  powerful technique, but has to be used carefully and not over-used.
  Otherwise, you will often get unintended results when you start to
  compute with objects from the new class.

  As shown in the examples below, the simplest and safest form of
  inheritance is to start with an explicit class, with some slots, that
  does not extend anything else.  It only does what we say it does.

  Then extensions will add some new slots and new behavior.

  Another variety of extension starts with one of the basic classes,
  perhaps with the intension of modifying R's standard behavior for that
  class.  Perfectly legal and sometimes quite helpful, but you may need
  to be more careful in this case:  your new class will inherit much of
  the behavior of the basic (informally defined) class, and the results
  can be surprising.  Just proceed with caution and plenty of testing.

  As an example, the class \code{"matrix"} is included in the
  pre-defined classes, to behave essentially as matrices do without
  formal class definitions.  Suppose we don't like all of this; in
  particular, we want the default matrix to have 0 rows and columns (not
  1 by 1 as it is now).
  
  \code{setClass("myMatrix", "matrix", prototype = matrix(0,0,0))}
  
  The arguments above illustrate two short-cuts relevant to such
  examples.  We abbreviated the \code{representation} argument to the
  single superclass, because the new class doesn't add anything to the
  representation of class \code{"matrix"}.  Also, we provided an object
  from the superclass as the prototype, not a list of slots.
}

\references{
  The web page \url{http://www.omegahat.org/RSMethods/index.html}
  is the primary documentation.

  The functions in this package emulate the facility for classes
  and methods described in
  \emph{Programming with Data}, (John M. Chambers, Springer, 1998).
  See this book for further details and examples.
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
}
## A simple class with two slots
setClass("track",
            representation(x="numeric", y="numeric"))
## A class extending the previous, adding one more slot
setClass("trackCurve",
            representation("track", smooth = "numeric"))
## A class similar to "trackCurve", but with different structure
## allowing matrices for the "y" and "smooth" slots
setClass("trackMultiCurve", representation(x="numeric", y="matrix", smooth="matrix"),
          prototype = list(x=numeric(), y=matrix(0,0,0), smooth= matrix(0,0,0)))
##
## Suppose we want  trackMultiCurve to be like trackCurve when there's only
## one column
## First, the wrong way. 
try(setIs("trackMultiCurve", "trackCurve",
  test = function(obj) {ncol(slot(obj, "y")) == 1}))

## why didn't that work?  You can only override the slots "x", "y", and "smooth"
## if you provide an explicit coerce function to correct any inconsistencies:

setIs("trackMultiCurve", "trackCurve",
  test = function(obj) {ncol(slot(obj, "y")) == 1},
  coerce = function(obj) { new("trackCurve", x = slot(obj, "x"),
        y = as.numeric(slot(obj,"y")), smooth = as.numeric(slot(obj, "smooth")))})
\testonly{
tMC <- new("trackMultiCurve")
is.matrix(slot(tMC, "y"))
is.matrix(slot(tMC, "smooth"))
setClass("myMatrix", "matrix", prototype = matrix(0,0,0))
nrow(new("myMatrix")) # 0
nrow(new("matrix")) # 1
## simple test of prototype data
xxx <- rnorm(3)
setClass("xNum", representation(x = "numeric"), prototype = list(x = xxx))
stopifnot(identical(new("xNum")@x, xxx))
}


}

\seealso{\code{\link{Methods}}, \code{\link{setSClass}}}
\keyword{programming}
\keyword{classes}
\keyword{methods}
