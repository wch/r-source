\name{ReferenceClasses}
\alias{ReferenceClasses}
\alias{setRefClass}
\alias{refClassFields}
\alias{refClassMethods}
\alias{refClassRepresentation-class}
\alias{refClass-class}
\alias{refObject-class}
\alias{classMethodDef-class}
\alias{show,classMethodDef-method}
\alias{show,refClassRepresentation-method}
\title{
Creating Classes With Fields Treated by Reference
}
\description{
A call to \code{setRefClass} defines an \R class with
fields that
are accessed by reference and with class methods to be invoked on objects
from the class, in the style of languages such as Java and
C++, commonly referred to as OOP languages.
The application interface provided is designed to deal consistently
with such classes whether implemented entirely in \R{} or through an
interface to another language.
}
\section{NOTE:}{
The software described here is an initial version.  The eventual goal
is to support reference-style classes with software in \R itself
or using inter-system interfaces.  The current implementation (\R
version 2.12.0) is preliminary and subject to change, and currently
includes only the \R-only implementation.  Developers are encouraged
to experiment with the software, but the description here is more than
usually subject to change.
}
\usage{
setRefClass(Class, fieldClasses = , contains = , classMethods =,
   fieldPrototypes =, basicRefClass = "stdRefClass",
   refClassName = Class, where =, ...)

refClassFields(Class)
refClassMethods(Class)
}
\arguments{
  \item{Class}{
character string name for the class.
}
  \item{fieldClasses}{
a named list of the fields, to be handled by reference semantics (see
the corresponding section below).  Note that these are distinct from
the slots, if any, in the object.  Slots are, as always, handled by
standard \R{} object management.
}
  \item{contains}{
optional vector of superclasses for this class.  If a superclass is
also a reference class, the fields and class methods will be inherited.
}
  \item{classMethods}{
a named list of function definitions that can be invoked on objects
from this class.  See the section on class methods below, and in
particular the comments on accessor methods for fields.
}
  \item{fieldPrototypes}{
an optional named list of prototype objects for a subset of the
fields.  By default, each field will be initialized to the prototype
object of the corresponding class.
}
  \item{basicRefClass}{
the name of a reference class from which the new class expects to inherit methods
to manage the reference semantics.
The design goal is to define distinct implementations of reference
through a set of basic reference classes.
The default, \code{"stdRefClass"}, implements reference objects
entirely in \R{} (currently by using environments); other choices of
basic reference class might use an interface to another language.  See
the section on relations to other languages.
}
  \item{refClassName}{
optionally, a name for the class to be used in interfacing to another
language, if for some reason that name is not suitable as an \R{}
class name.  By default, and usually, the name provided by
argument \code{Class} is used.
}
  \item{where}{
the environment in which to store the class definition.  Defaults to
the package namespace or environment for code that is part of an \R{}
package, and to the global environment for code sourced directly at
the session top level.
}
  \item{\dots}{
other arguments to be passed to \code{\link{setClass}}.
}
}

\section{Reference Objects}{
Normal objects in \R are passed as arguments consistently with
functional programming semantics; that is, changes made to an object
passed as an argument are local to the function call.  The object that
supplied the argument is unchanged.

Functional object semantics (sometimes called pass-by-value) are
suitable for many statistical computations and are implicit, for
example, in the basic \R software for fitting statistical models.
In other situations, one would like all the code dealing with an
object to see the exact same content, so that changes made in any
function call would be reflected everywhere.
This is often suitable if the object has some \dQuote{objective}
reality, such as a window in a user interface.

In addition, a number of languages, such as Java and C++ and many
others, support a version of classes and methods assuming reference
semantics.
The basic programming mechanism is to invoke a method on an object,
different in several respects from a function call in \R.
In the \R syntax we use for this operation, one invokes a method,
\code{m1} say, on an object \code{x} by the expression
\code{x$m1(...)}.  It's not a logical necessity that such %$
class/method systems use reference semantics, essentially all of them
do.
In particular, they usually include methods that modify the content of
the object.

The goal of the software described here is to provide a uniform
programming style in \R to deal with such software, whether it is
implemented directly in \R or through an interface to one of the other
languages.
Several implications follow for the \R software.
Methods in this model are associated with the object, or more
precisely with the class of the object, as opposed to methods in a
functional class/method system, which are fundamentally associated
with the function (in \R, for example, a generic function in an \R
session has a table of all its currently known methods).

This relation of methods to objects is one reason such systems are
often referred to as Object Oriented Programming (OOP).  While there
is nothing incorrect about the term, it has been the cause of
confusion, particularly when programmers come to \R from one of the
other systems.
In a normal use of the words, the functional class/method system in \R
is just as object-oriented as Java, say, if not more so.
But the systems are fundamentally different.
To avoid propagating the confusion, we will avoid the term OOP
throughout this document.
If we need to make the distinction, we will refer to class-based
methods versus functional methods.
}
\section{Fields and Class Methods}{
All the widely used languages implementing class-based methods provide
for named \dQuote{pieces} of an object.
The term for these pieces varies with the language; in the \R
implementations they will be called \emph{fields}.
In many languages, a field is accessed by using the operator
\code{"."}, but this is not available to us in \R because the
character can be part of a name (blame it on the Unix system whose
convention of starting hidden files with \code{"."} was taken over in
S).

Instead, and rather than introduce yet another operator into the
grammar, we use the \code{"$"} operator. %$
A field is extracted by this operator and modified by the operator
appearing on the left of an assignment.
In terms of implementation, any class representing reference objects
of this form will have or inherit methods for \code{"$"} and
\code{"$<-"}.
Objects from reference classes will inherit
these methods from a basic reference class, either using \R (the
standard being class \linkS4class{stdRefClass}) or interfacing to
another system.

Similarly, a class method is invoked on an object using the \code{$}
operator.  To invoke a class method \code{undo} on an object \code{x},
call \code{x$undo()}.
}

\section{Writing Class Methods}{
Class methods are functions supplied as elements of a named list as
the argument \code{classMethods} in a call to \code{setRefClass}.
They are written as ordinary \R functions but have some special
features and restrictions.
The body of the function can contain calls to any other class method,
including those inherited from other reference classes.

Class methods also need to refer to fields in the object.
A number of
class-based systems recommend or enforce \emph{accessor methods}
corresponding to each field.
In the \R version presented here (and fairly often elsewhere
as well), a field named \code{abc} of an object \code{x} would be
extracted by \code{x$getAbc()} and assigned by
\code{x$setAbc(value)}.
Aside from other reasons, using accessor methods in the standard \R
implementation allows class methods to be defined independently of the
precise mechanism currently used to implement fields (see the
implementation section below).
A requirement for defining reference classes in \R is that methods be
available to get any field and to set any field, although an error
may be generated if a field is defined to be read-only.

For reference classes using the \linkS4class{stdRefClass} basic
reference class, the use of accessor methods is optional.
An alternative is to use the field name directly for getting the field
and assignments of the field name using the
non-local assignment operator, \code{<<-}, to set the field.  See the
second version of class methods in the example below.
Notice that non-local assignment is required:  a local assignment with
the \code{<-} operator just creates a local object in the function
call, as it would in any \R function.

Direct access to fields in a class method usually gives a simpler and slightly faster
implementation.
However, using accessor methods is required if the reference
object is a proxy for an object in another language.
Using them in the \R-only case as well has the advantage of being compatible
with the external case, so the same method definition applies to both.
That's an important advantage in principle, because
one of the main goals for the software described here is to provide a
consistent programming interface both for \R-only software and for
inter-system interfaces.

Class methods should be kept simple; if they need to do some
specialized \R computation, put that into a separate, ordinary
function and write the class method to call that.
Specifically, class methods can not use special features of the
enclosing environment mechanism.
Class methods can not themselves be generic functions; if you want
additional functional method dispatch, write a separate generic
function and call that from the class method.
The implementation section explains the reason for these restrictions.

The entire object can be referred to in a class method by the reserved
name \code{.self}, as shown in the \code{save=} class method of the
example.

Class methods include inherited class methods.  See the section on
inheritance for a (tentative) approach to addressing inherited
versions of methods.
}
\section{Inheritance}{
Reference classes inherit from other reference classes by using the
standard \R inheritance; that is, by including the superclasses in the
\code{contains=} argument when creating the new class.
Non-reference classes can also be included in the \code{contains=} argument.  The class definition
mechanism treats reference and non-reference superclasses slightly differently.
If the contained reference classes themselves have reference
superclasses, these will be moved ahead of any non-reference
superclasses in the class definition (otherwise the ordering of
superclasses may be ambiguous).
The names of the reference superclasses are in slot
\code{refSuperClasses} of the class definition.

Class fields are inherited.  Currently the new class definition can
override any inherited fields:  the previous field definition is no
longer available.

Class methods are inherited and installed in the same way as directly
specified class methods.
If a directly specified method overrides an inherited one, the current
mechanism is to include a \code{".prev"} version of the method.  So if
a class inherited an \code{edit()} method but also defined one
directly, the previous method can be called as \code{edit.prev()}.
See the matrix viewer example below.
One serious drawback is that only one level of inherited method can be used.
(As with other details, we don't guarantee that this mechanism will
persist if a better approach comes along.)

}
\section{Implementation}{
Reference class definitions need to store the definition of the fields
and also the class methods (since these belong to the class in the
class-based paradigm).
These and some other properties are defined in the class
\code{refClassRepresentation}, which is a subclass of the ordinary
class definition class, \linkS4class{classRepresentation}.
As of version 2.12.0 of \R, the details of this class are not part of
the API.
Helper functions \code{refClassFields} and \code{refClassMethods} will
extract class field and method information.
It's a bad idea to count on the specific slots as currently documented
(to satisfy package checking).

The implementation provides for field and class method access through
the \code{$} operator. %$
This includes replacement of field values in a reference class object.
Class method definitions are also supported as documented here.

The implementation for both purposes works by delivering a function
object for the method with an environment containing the fields and
class methods as objects, specifically
the environment of the object itself.
The \code{initialize} method for standard reference class objects
installs an environment in the object with the initial contents of the
class fields (either the defaults from the prototype or values
supplied explicitly in the call to \code{\link{new}}).
Class methods are inserted into the environment when they are called,
including other class methods they may call.
These dependencies are searched heuristically when the reference class
is defined, using the \code{findGlobals} function in recommended
package \code{codeTools}.
This recommended package must be available to define a reference class
(but is not required to use reference classes defined in an installed package).
}
\value{
\code{setRefClass} returns the class definition, invisibly, but the function is mainly
called for the side effect of assigning the definition, usually as
part of an \R{} package.

\code{refClassFields} returns a list of the classes for all the class
fields.

\code{refClassMethods} returns a list of the class method functions,
including those inherited and accessor methods generated.

}
\author{
John Chambers
}


 \examples{
## a simple editor for matrix objects.  Method  edit() changes some
## range of values; method undo() undoes the last edit.
setRefClass("matrixEditor",
      fieldClasses = list( data = "matrix",
        edits = "list"),
      classMethods = list(
        edit = function(i, j, value) {
          x <- getData()
          backup <-
            list(i, j, x[i,j])
          setEdits(c(list(backup),
            getEdits()))
          x[i,j] <- value
          setData(x)
          invisible(value)
        },
        undo = function() {
          prev <- getEdits()
          if(length(prev)) prev <- prev[[1]]
          else stop("No more edits to undo")
          edit(prev[[1]], prev[[2]], prev[[3]])
          ## trim the edits list
          setEdits(
            getEdits()[-(1:2)])
          invisible(prev)
        },
        save = function(file) {
          base::save(.self, file = file)
        }
      )
    )
xMat <- matrix(1:12,4,3)
xx <- new("matrixEditor", data = xMat)
xx$edit(2, 2, 0)
xx$data
xx$undo()
stopifnot(all.equal(xx$data, xMat))
\dontshow{
tf <- tempfile()
xx$save(tf) #$
load(tf)
unlink(tf)
stopifnot(identical(xx$data, .self$data))
}
## A version of the classMethods= argument above that
## is a little simpler and faster but only works with
## the implementation in R, not with interfaces:
localMethods <- list(
     edit = function(i, j, value) {
         backup <-
             list(i, j, data[i,j])
         edits <<- c(list(backup),
                     edits)
         data[i,j] <<- value
         invisible(value)
     },
     undo = function() {
         prev <- getEdits()
         if(length(prev)) prev <- prev[[1]]
         else stop("No more edits to undo")
         edit(prev[[1]], prev[[2]], prev[[3]])
         ## trim the edits list
         length(edits) <<- length(edits) - 2
         invisible(prev)
     },
     save = function(file) {
         base::save(.self, file = file)
     }
)
setRefClass("matrixEditor2",
      fieldClasses = list( data = "matrix",
        edits = "list"),
      classMethods = localMethods)

## Inheriting a reference class:  a matrix viewer
setRefClass("matrixViewer", contains = "matrixEditor",
    classMethods = list( view = function() matplot(data),
        edit = # invoke previous method, then replot
          function(i, j, value) {
            edit.prev(i, j, value)
            view()
          }))

\dontshow{
ff = new("matrixEditor2", data = xMat)
ff$edit(2,2,0)
ff$data
ff$undo()
stopifnot(all.equal(ff$data, xMat))
removeClass("matrixEditor")
removeClass("matrixEditor2")
removeClass("matrixViewer")
}
}
\keyword{ programming }
\keyword{ classes }
