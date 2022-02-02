% File src/library/methods/man/setClass.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2017 R Core Team
% Distributed under GPL 2 or later

\name{setClass}
\alias{setClass}
\alias{classGeneratorFunction-class}
\title{Create a Class Definition}
\description{
  Create  a class definition and return a generator function to create
  objects from the class.  Typical usage will be
  of the style:

   \code{myClass <- setClass("myClass", slots= ...., contains =....)}

 where the first argument is the name of the new class and, if supplied, the arguments    
  \code{slots=} and \code{contains=} specify the slots
  in the new class and existing classes from which the new class
  should inherit.  Calls to \code{setClass()} are normally found in the
  source of a package; when the package is loaded the class will be
  defined in the package's namespace.  Assigning the generator
  function with the name of the class is  convenient for users, but
  not a requirement.
}
\usage{
setClass(Class, representation, prototype, contains=character(),
         validity, access, where, version, sealed, package,
         S3methods = FALSE, slots)
}
\arguments{
  \item{Class}{character string name for the class.}
  \item{slots}{  The names and classes for the slots in the new class.  This argument
      must be supplied by name, \code{slots=}, in the call, for back compatibility
      with other arguments no longer recommended.

      The argument must be  vector with a names attribute, the names being those of the slots in
      the new class.  Each element of the vector specifies an
      existing class; the corresponding slot must be from this class
      or a subclass of it.  Usually, this is a character vector
      naming the classes.  It's also legal for the elements of the
      vector to be class representation objects, as returned by \code{\link{getClass}}.


    As a limiting
    case,  the argument may be an unnamed character
    vector;  the elements are  taken as slot names and all slots have
    the unrestricted class \code{"ANY"}. 
  }
  \item{contains}{ A vector specifying existing classes from which
      this class should inherit. The new class will have all the slots
      of the superclasses, with the same requirements on the classes
      of these slots.  This argument
      must be supplied by name, \code{contains=}, in the call, for back compatibility
      with other arguments no longer recommended.
      
      See the section \sQuote{Virtual Classes} for the special
      superclass  \code{"VIRTUAL"}.
      
    }
  \item{prototype, where, validity, sealed, package}{
      \emph{These arguments are currently allowed, but either they are unlikely to be
        useful or there are modern alternatives that are preferred.}

     \code{prototype}: supplies an object with the default
    data for the slots in this class.  A more flexible approach is to
    write a method for \code{\link{initialize}()}.

  \code{where}: supplies an environment in which to store the definition.
    Should not be used:  For calls to
    \code{setClass()} appearing in the source code for a package the
    definition will be stored in the namespace of the package.

  \code{validity}: supplied a validity-checking method
    for objects from this class.  For clearer code, use a separate
    call to \code{\link{setValidity}()}.

  \code{sealed}: if \code{TRUE}, the class definition will be sealed,
    so that another call to \code{setClass} will fail on this class
    name.  But the definition is automatically sealed after the
    namespace is loaded, so explicit sealing it is not needed.

  \code{package}: supplies an optional package name for the class, but
  the class attribute should be  the package in which the class
  definition is assigned, as it is by default.
  }

  \item{representation, access, version, S3methods }{\emph{All these
    arguments are deprecated from version 3.0.0 of \R and should be
    avoided}.

    \code{representation} is an argument inherited from S that
    included both \code{slots} and \code{contains}, but the use of
    the latter two arguments is clearer and recommended.

    \code{access} and \code{version} are included for
    historical compatibility with S-Plus, but ignored.
 
    \code{S3methods} is a flag indicating that old-style methods
    will be written involving this class; ignored now.
  }
}

\value{
  A generator function suitable for creating objects from the class is
  returned, invisibly.  A call to this function generates a call to
  \code{\link{new}} for the class.  The call takes any number of arguments,
  which will be passed on to the initialize method.  If no
  \code{initialize} method is defined for the class or one of its
  superclasses, the default method expects named arguments with the
  name of one of the slots and unnamed arguments that are objects from
  one of the contained classes.

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
superclass information specifies which methods apply indirectly,
through inheritance.  See \link{Methods_Details} for inheritance in method
selection.


The slots in a class definition will be the union of all the slots
specified directly by \code{slots} and all the slots in all
the contained classes.
There can only be one slot with a given name.
A class may override the definition of a slot with a given name, but
\emph{only} if the newly specified class is a subclass of the
inherited one.
For example, if the contained class had a slot \code{a} with class
\code{"ANY"}, then a subclass could specify \code{a} with class
\code{"numeric"},
but if the original specification for the slot was class
\code{"character"}, the new call to \code{setClass} would generate an error.



  Slot names \code{"class"} and \code{"Class"} are not allowed.
  There are other slot names with a special meaning; these names start with
  the \code{"."} character.  To be safe, you should define all of
  your own slots with names starting with an alphabetic character.

Some inherited classes will be treated specially---object types, S3
classes and a few special cases---whether inherited
directly or indirectly.  See the next three sections.
}

\section{Virtual Classes}{

      Classes exist for which no actual objects can be created, the
      \emph{virtual} classes.


      The most common and useful form of virtual class is the \emph{class
        union}, a virtual class that is defined in a call to
      \code{\link{setClassUnion}()} rather than a call to
      \code{setClass()}.
      This call lists the \emph{members} of the union---subclasses
      that extend the new class.
      Methods that are written with the class union in the signature
      are eligible for use with objects from any of the member classes.
      Class
      unions can include as members classes whose
      definition is otherwise sealed, including basic \R data types.

      Calls to \code{setClass()} will also create a virtual class,
      either when only the \code{Class} argument is supplied (no slots
      or superclasses) or when the \code{contains=} argument includes
      the special class name \code{"VIRTUAL"}.

      In the latter case, a
      virtual class may include
      slots to provide some common behavior without fully defining
      the object---see the class \code{\linkS4class{traceable}} for an
      example.
      Note that  \code{"VIRTUAL"} does not carry over to subclasses; a
      class that contains a virtual class is not itself automatically virtual.

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
This appears as a pseudo-slot, \code{".Data"} and can be treated as a
slot but actually determines
the type of objects from this slot.

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
reserved slot, \code{".xData"}.
See the
example for class \code{"stampedEnv"} below.
An object from such a class does \emph{not} have a \code{".Data"} pseudo-slot.

For most computations, these classes behave transparently as if they
inherited directly from the anomalous type.
S3 method dispatch and the relevant \code{as.}\emph{type}\code{()}
functions should behave correctly, but code that uses the type of the
object directly will not.
For example, \code{as.environment(e1)} would work as expected with the
\code{"stampedEnv"} class, but \code{typeof(e1)} is \code{"S4"}.

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
\code{\link{setOldClass}}.  For details, see \code{\link{setOldClass}}
and Section 10.8 of the reference.

}

\section{Classes and Packages}{

Class definitions normally belong to packages (but can be defined in
the  global environment as well, by evaluating the expression on the
command line or in a file sourced from the command line).
The corresponding package name is part of the class definition; that
is, part of the \code{\linkS4class{classRepresentation}} object holding that
definition.  Thus, two classes with the same name can exist in
different packages, for most purposes.

When a class name is supplied for a slot or a superclass in a call to
\code{setClass}, a
corresponding class definition will be found, looking from the
namespace of the current package, assuming the call in question appears directly in the source for the
package, as it should to avoid ambiguity.
The  class definition
must be already defined in this package, in the imports directives of
the package's \code{DESCRIPTION} and
\code{NAMESPACE} files or in the basic classes defined by the methods package.
(The \sQuote{methods} package must be included in the imports directives
for any package that uses
S4 methods and classes, to satisfy the
\code{"CMD check"} utility.)

If a package imports two classes of the same name from separate packages, the \code{\link{packageSlot}}
of the \code{name} argument needs to be set to the package name of the
particular class.
This should be a rare occurrence.
}

\references{
 Chambers, John M. (2016)
 \emph{Extending R},
  Chapman & Hall.
(Chapters 9 and 10.)
}

\seealso{
  \code{\link{Classes_Details}} for a general discussion of classes,
  \code{\link{Methods_Details}} for an analogous discussion of methods,
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
