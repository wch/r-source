% File src/library/methods/man/setClass.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{setClass}
\alias{setClass}
\alias{classGeneratorFunction-class}
\title{Create a Class Definition}
\description{
  Create  a class definition, specifying the representation (the
  slots) and/or the classes contained in this one (the superclasses),
  plus other optional details.  As a side effect, the class definition
  is stored in the specified environment.  A generator function
  is returned as the value of \code{setClass()}, suitable for creating
  objects from the class if the class is not virtual.  Of the many
  arguments to the function only \code{Class},
  \code{slots=} and \code{contains=} are usually needed.
}
\usage{
setClass(Class, representation, prototype, contains=character(),
         validity, access, where, version, sealed, package,
         S3methods = FALSE, slots)
}
\arguments{
  \item{Class}{character string name for the class.}
  \item{slots}{ a named list or named character vector.
    The names are the names of the slots in the new class and
    the elements are the character string names of  the
    corresponding classes.

    In rare cases where there is ambiguity about the class of a slot,
    because two classes of the same name are imported from different
    packages, the corresponding element of the argument must have a
    \code{"package"} attribute to disambiguate the choice.

    It is allowed to provide an unnamed character vector as a limiting
    case, with the elements taken as slot names and all slots having
    the unrestricted class \code{"ANY"}.
  }
  \item{contains}{ the names (and optionally package slots) for the
    \emph{superclasses} of this class.  The special superclass
    \code{"VIRTUAL"} causes the new class to be created as a
    virtual class; see the section on virtual classes in \link{Classes}.}
  \item{prototype}{ an object providing the default
    data for the slots in this class.  By default, each will be the
    prototype object for the superclass.  If provided, using a call to
    \code{\link{prototype}} will carry out some checks. }
  \item{where}{ the environment in which to store the definition.
    Should not be supplied in standard use.  For calls to
    \code{setClass()} appearing in the source code for a package, will
    default to the namespace of the package.  For calls typed or sourced
    at the top level in a session, will default to the global environment.
  }
  \item{validity}{ if supplied, should be a validity-checking method
    for objects from this class (a function that returns \code{TRUE} if
    its argument is a valid object of this class and one or more strings
    describing the failures otherwise).  See \code{\link{validObject}}
    for details.}
  \item{S3methods, representation, access, version }{All these
    arguments are deprecated from version 3.0.0 of \R and should be
    \emph{avoided}.

    \code{S3methods} is a flag indicating that old-style methods
    will be written involving this class.  Modern versions of \R
    attempt to match formal and old-style methods consistently, so
    this argument is largely irrelevant.

    \code{representation} is an argument inherited from S that
    included both \code{slots} and \code{contains}, but the use of
    the latter two arguments is clearer and recommended.

    \code{access} and \code{version} are included for
    historical compatibility with S-Plus, but ignored.
  }
  \item{sealed}{ if \code{TRUE}, the class definition will be sealed,
    so that another call to \code{setClass} will fail on this class name.
  }
  \item{package}{ an optional package name for the class.  Should very
      rarely be used. By default
    the name of the package in which the class definition is assigned.
  }
}

\value{
  A generator function suitable for creating objects from the class is
  returned, invisibly.  A call to this function generates a call to
  \code{\link{new}} for the class.  The call takes any number of arguments,
  which will be passed on to the initialize method.  If no
  \code{initialize} method is defined for the class or one of its
  superclasses, the default method expects named arguments with the
  name of one of the slots.

  Typically the generator function is assigned the name of the class,
  for programming clarity.  This is not a requirement and objects
  from the class can also be generated directly from
  \code{\link{new}}.  The advantages of the generator function are a
  slightly simpler and clearer call, and that the call will contain
  the package name of the class (eliminating any ambiguity if two
  classes from different packages have the same name).

  If the class is virtual, an attempt to generate an object  from
  either the generator or \code{new()}
  will result in an error.
}
\section{Basic Use: Slots and Inheritance}{
The two essential arguments other than the class name are
\code{slots} and \code{contains}, defining the explicit slots
and the inheritance (superclasses). Together, these arguments define
all the information in an object from this class; that is, the names
of all the slots and the classes required for each of them.

The name of the class determines
which methods apply directly to objects from this class.  The
inheritance information specifies which methods apply indirectly,
through inheritance.  See \link{Methods}.

The slots in a class definition will be the union of all the slots
specified directly by \code{slots} and all the slots in all
the contained classes.
There can only be one slot with a given name; specifically, the
direct and inherited slot names must be unique.
That does not, however, prevent the same class from being inherited
via more than one path.

One kind of element in the \code{contains=} argument is special, specifying one of the \R
object types or one of a few other special \R types (\code{matrix} and
\code{array}).
See the section on inheriting from object types, below.


  Slot names \code{"class"} and \code{"Class"} are not allowed.
  There are other slot names with a special meaning; these names start with
  the \code{"."} character.  To be safe, you should define all of
  your own slots with names starting with an alphabetic character.
}



\section{Inheriting from Object Types}{
In addition to containing other S4 classes, a class definition can
contain either an S3 class (see the next section) or a built-in R pseudo-class---one
of the \R
object types or one of the special \R pseudo-classes \code{"matrix"} and
\code{"array"}.
A class can contain at most one of the object types, directly or indirectly.
When it does, that contained class determines the \dQuote{data part}
of the class.

Objects from the new class try to inherit the built in
behavior of the contained type.
In the case of normal \R data types, including vectors, functions and
expressions, the implementation is relatively straightforward.
For any object \code{x} from the class,
\code{typeof(x)} will be the contained basic type; and a special
pseudo-slot, \code{.Data}, will be shown with the corresponding class.
See the \code{"numWithId"} example below.

Classes may also inherit from \code{"vector"}, \code{"matrix"} or
\code{"array"}.
The data part of these objects can be any vector data type.

For an object from any class that does \emph{not} contain one of these
types or classes,
\code{typeof(x)} will be \code{"S4"}.

Some \R data types do not behave normally, in the sense that they are
non-local references or other objects that are not duplicated.
Examples include those corresponding to classes \code{"environment"}, \code{"externalptr"}, and \code{"name"}.
These can not be the types for objects with user-defined
classes (either S4 or S3) because setting an attribute overwrites the
object in all contexts.
It is possible to define a class that inherits from such types,
through an indirect mechanism that stores the inherited object in a
reserved slot.
See the
example for class \code{"stampedEnv"} below.
S3 method dispatch and the relevant \code{as.}\emph{type}\code{()}
functions should behave correctly, but code that uses the type of the
object directly will not.

Also, keep in mind that the object passed to low-level computations
will be the underlying object type, \emph{without} any of the slots
defined in the class.
To return the full information, you will usually have to define a
method that sets the data part.

}

\section{Inheriting from S3 Classes}{
Old-style S3 classes have no formal definition.  Objects are
\dQuote{from} the class when their class attribute contains the
character string considered to be the class name.

Using such classes with formal classes and methods is necessarily a
risky business, since there are no guarantees about the content of the
objects or about consistency of inherited methods.
Given that, it is still possible to define a class that inherits from
an S3 class, providing that class has been registered as an old class
(see \code{\link{setOldClass}}).

Broadly speaking, both S3 and S4 method dispatch try to behave
sensibly with respect to inheritance in either system.
Given an S4 object, S3 method dispatch and the \code{\link{inherits}}
function should use the S4 inheritance information.
Given an S3 object, an S4 generic function will dispatch S4 methods
using the S3 inheritance, provided that inheritance has been declared via
\code{\link{setOldClass}}.

}

\section{Classes and Packages}{

Class definitions normally belong to packages (but can be defined in
the  global environment as well, by evaluating the expression on the
command line or in a file sourced from the command line).
The corresponding package name is part of the class definition; that
is, part of the \code{classRepresentation} object holding that
definition.  Thus, two classes with the same name can exist in
different packages, for most purposes.

When a class name is supplied for a slot or a superclass in a call to
\code{setClass}, a
corresponding class definition will be found, looking from the
namespace of the current package, assuming the call in question appears directly in the source for the
package, as it should to avoid ambiguity.
The  class definition
must be found in the namespace of the current package, in the imports for that
namespace or in the basic classes defined by the methods package.
(The methods package must be included in the \code{Depends} directive
of the package's \code{"DESCRIPTION"} file in order for the
\code{"CMD check"} utility to find these classes.)

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
 if(isClass("trackCurve"))      removeClass("trackCurve")
 if(isClass("track"))           removeClass("track")
}
## A simple class with two slots
track <- setClass("track", slots = c(x="numeric", y="numeric"))
## an object from the class
t1 <- track(x = 1:10, y = 1:10 + rnorm(10))

## A class extending the previous, adding one more slot
trackCurve <- setClass("trackCurve",
		slots = c(smooth = "numeric"),
		contains = "track")

## an object containing a superclass object
t1s <- trackCurve(t1, smooth = 1:10)

## A class similar to "trackCurve", but with different structure
## allowing matrices for the "y" and "smooth" slots
setClass("trackMultiCurve",
         slots = c(x="numeric", y="matrix", smooth="matrix"),
         prototype = list(x=numeric(), y=matrix(0,0,0),
                          smooth= matrix(0,0,0)))
## See ?setIs for further examples using these classes

## A class that extends the built-in data type "numeric"

numWithId <- setClass("numWithId", slots = c(id = "character"),
         contains = "numeric")

numWithId(1:3, id = "An Example")

## inherit from reference object of type "environment"
stampedEnv <- setClass("stampedEnv", contains = "environment",
                       slots = c(update = "POSIXct"))
setMethod("[[<-", c("stampedEnv", "character", "missing"),
   function(x, i, j, ..., value) {
       ev <- as(x, "environment")
       ev[[i]] <- value  #update the object in the environment
       x@update <- Sys.time() # and the update time
       x})


e1 <- stampedEnv(update = Sys.time())

e1[["noise"]] <- rnorm(10)

\dontshow{
tMC <- new("trackMultiCurve")
is.matrix(slot(tMC, "y"))
is.matrix(slot(tMC, "smooth"))
setClass("myMatrix", "matrix", prototype = matrix(0,0,0))
nrow(new("myMatrix")) # 0
nrow(new("matrix")) # 1
## simple test of prototype data
xxx <- stats::rnorm(3)
setClass("xNum", slots = c(x = "numeric"), prototype = list(x = xxx))
stopifnot(identical(new("xNum")@x, xxx))

removeClass("xNum")
removeClass("myMatrix")

## The following should not be needed.  But make check removes all files
## between example files, in a crude way that does not cause the class
## information to be reset.  There seems no way to detect this, so we
## have to remove classes ourselves

removeClass("trackMultiCurve")
removeClass("trackCurve")
removeClass("track")
}%dont show
}
\keyword{programming}
\keyword{classes}
\keyword{methods}
