\name{ReferenceClasses}
\alias{ReferenceClasses}
\alias{setRefClass}
\alias{refClassFields}
\alias{refClassMethods}
\alias{initFieldArgs}
\alias{activeBindingFunction-class}
\alias{defaultBindingFunction-class}
\alias{uninitializedField-class}
\alias{refClassRepresentation-class}
\alias{refObjectGenerator-class}
\alias{refClass-class}
\alias{refObject-class}
\alias{refMethodDef-class}
\alias{SuperClassMethod-class}
\alias{show,refMethodDef-method}
\alias{show,refClassRepresentation-method}
\title{
Creating Classes With Fields Treated by Reference (OOP-style)
}
\description{
The software described here supports objects with
fields that
are accessed by reference and with  methods to be invoked on such objects, in the style of languages such as Java and
C++, commonly referred to as OOP languages.
The software can be used to program in \R directly or in combination
with an interface to such a language, allowing \R-written methods to
extend the interface.
The function \code{setRefClass} supports the \R software; the
intention is that inter-system interfaces will use the software
transparently to allow \R methods.
}
\usage{
setRefClass(Class, fields = , contains = , methods =,
     where =, ...)

refClassFields(Class)
refClassMethods(Class)

}
\arguments{
  \item{Class}{
character string name for the class.
}
  \item{fields}{
either a character vector of field names or
a named list of the fields.  The resulting fields will be accessed with reference semantics (see
the  section on dQuote{Reference Objects}).  If the argument is a list, the
elements of the list can be the character string name of a class, in
which case the field must be from that class or a subclass.
Alternatively, the element can be an accessor function that returns
the field if called with no argument or sets it if given a single
arguments.  See the section on \dQuote{Accessor Functions}.


Note that fields are distinct from
the slots, if any, in the object.  Slots are, as always, handled by
standard \R{} object management.  Slots for the class can be included
(as the \code{representation=} argument) in the \dots argument.
}
  \item{contains}{
optional vector of superclasses for this class.  If a superclass is
also a reference class, the fields and class-based methods will be inherited.
}
  \item{methods}{
a named list of function definitions that can be invoked on objects
from this class.  These can also be created by invoking the
\code{$methods} method on the generator object returned. %$
See the section on \dQuote{Writing Reference Methods} for details.
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
Normal objects in \R are passed as arguments in function calls consistently with
functional programming semantics; that is, changes made to an object
passed as an argument are local to the function call.  The object that
supplied the argument is unchanged.

The functional model (sometimes called pass-by-value) is
suitable for many statistical computations and is implicit, for
example, in the basic \R software for fitting statistical models.
In some other situations, one would like all the code dealing with an
object to see the exact same content, so that changes made in any
computation would be reflected everywhere.
This is often suitable if the object has some \dQuote{objective}
reality, such as a window in a user interface.

In addition, commonly used languages, including Java, C++ and many
others, support a version of classes and methods assuming reference
semantics.
Typically, the basic programming mechanism in these languages
is to invoke a method on an object, with
different interpretation from a function call in \R.
In the \R syntax we use for this operation, one invokes a method,
\code{m1} say, on an object \code{x} by the expression
\code{x$m1(...)}. %$
Objects in this programming model usually have named fields on which
the methods operate.
In particular, methods are usually included that modify the content of
the fields.

The goal of the software described here is to provide a uniform
programming style in \R to deal with such software, whether it is
implemented directly in \R or through an interface to one of the other
languages.
Several implications follow for the \R software.
Methods in this model are associated with the object, or more
precisely with the class of the object, as opposed to methods in a
function-based class/method system, which are fundamentally associated
with the function (in \R, for example, a generic function in an \R
session has a table of all its currently known methods).

This relation of methods to objects is one reason such systems are
often referred to as Object Oriented Programming (OOP).  While there
is nothing incorrect about the term, it has been the cause of
confusion, particularly when programmers come to \R from one of the
other systems.
In a normal use of the words, the function-based class/method system in \R
is just as object-oriented as Java, say, if not more so.
But the systems are fundamentally different.
To avoid propagating the confusion, we will avoid the term OOP
throughout this document.

Some terms are needed to distinguish the
OOP-style structure from the similar function-based classes and methods
structure.  The terms \dQuote{field} and
\dQuote{reference method} are used in distinction to \dQuote{slot}
and \dQuote{functional method} for the usual S4 classes.
There are  arguments for and
against most obvious terminology borrowed from other languages.  And,
as mentioned above, even the term \dQuote{OOP} is potentially
confusing.

In this document, \dQuote{method} unqualified usually means a
reference method.
}
\section{Fields and Reference Methods}{
All the widely used languages implementing reference methods provide
for named \dQuote{pieces} of an object.
The term for these pieces varies with the language; in the \R
implementations they will be called \emph{fields}.
In many languages, a field is accessed by using the operator
\code{"."}, but this is not available to us in \R because the
character can be part of a name (blame it on the Unix system whose
convention of starting hidden files with \code{"."} was taken over in
S).

Instead, and rather than introduce yet another operator into the
grammar, we use the \code{$} operator, %$
as has been done in a number of related \R packages, including
inter-system interfaces to languages such as Java and C++.
A field is extracted by this operator and modified by the operator
appearing on the left of an assignment.
In terms of implementation, any class representing reference objects
of this form will have or inherit methods for \code{"$"} and
\code{"$<-"}.
Objects from reference classes will inherit
these methods from a basic reference class, either using \R (the
standard being class \linkS4class{envRefClass}) or interfacing to
another system.

Similarly, a reference method is invoked on an object using the \code{$}
operator.  To invoke a reference method \code{undo} on an object \code{x},
call \code{x$undo()}.
}

\section{Writing Reference Methods}{
Reference methods are functions supplied as elements of a named list as
the argument \code{refMethods} in a call to \code{setRefClass}.
They are written as ordinary \R functions but have some special
features and restrictions.
The body of the function can contain calls to any other reference method,
including those inherited from other reference classes.

Reference methods also need to refer to fields in the object.
A number of
systems using this programming paradigm recommend or enforce \emph{accessor methods}
corresponding to each field.
In the \R version presented here (and fairly often elsewhere
as well), a field named \code{abc} of an object \code{x} would be
extracted by \code{x$getAbc()} and assigned by
\code{x$setAbc(value)}.
Aside from other reasons, using accessor methods in the standard \R
implementation allows class-based methods to be defined independently of the
precise mechanism currently used to implement fields (see the
implementation section below).
A requirement for defining reference classes in \R is that methods be
available to get any field and to set any field, although an error
may be generated if a field is defined to be read-only.

For reference classes using the \linkS4class{envRefClass} basic
reference class, the use of accessor methods is optional.
An alternative is to use the field name directly for getting the field
and assignments of the field name using the
non-local assignment operator, \code{<<-}, to set the field.  See the
second version of class-based methods in the example below.
Notice that non-local assignment is required:  a local assignment with
the \code{<-} operator just creates a local object in the function
call, as it would in any \R function.

Direct access to fields in a class-based method usually gives a simpler and slightly faster
implementation.
However, using accessor methods may be required if the reference
object is a proxy for an object in another language.
Using them in the \R-only case as well has the advantage of being compatible
with the external case, so the same method definition applies to both.
That's an important advantage in principle, because
one of the main goals for the software described here is to provide a
consistent programming interface both for \R-only software and for
inter-system interfaces.

Reference methods should be kept simple; if they need to do some
specialized \R computation, that computation should use a separate \R
function that is called from the reference method.
Specifically, methods can not use special features of the
enclosing environment mechanism.
Reference methods can not themselves be generic functions; if you want
additional function-based method dispatch, write a separate generic
function and call that from the method.
(The restrictions arise because the method's environment is used to
implement references: see the \dQuote{Implementation} section.)

The entire object can be referred to in a method by the reserved
name \code{.self}, as shown in the \code{save=} method of the
example.

The methods defined in the class include methods from superclasses, as
discussed in the next section.
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

Inherited methods are installed in the same way as directly
specified methods.
The code in a method can refer to  inherited methods in the same
way as directly specified methods.

A method may override a method of the same name in a superclass.  For
this purpose, a special method name will be provided, to be translated
into a call to the superclass method overridden.  This mechanism has
not yet been implemented, but the current plan is that
\code{callSuper(...)} will translate into a call to the superclass
version of the current method.
See the matrix viewer example below.

}
\section{Implementation}{
Reference class definitions need to store the definition of the fields
and also the class-based methods (since these belong to the class in the
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
class-based methods as objects, specifically
the environment of the object itself.
The \code{initialize} method for standard reference class objects
installs an environment in the object with the initial contents of the
class fields (either the defaults from the prototype or values
supplied explicitly in the call to \code{\link{new}}).
Methods are inserted into the environment when they are called,
including other methods they may call.
These dependencies are searched heuristically when the reference class
is defined, using the \code{findGlobals} function in recommended
package \code{codeTools}.
This recommended package must be available to define a reference class
(but is not required to use reference classes defined in an installed package).
}

\section{Reference Class Generator Objects}{
The definition of a reference class, by a call to \code{setRefClass},
returns a \dQuote{generator} object for that class.
This object is itself a reference object (of class
\code{"refObjectGenerator"}).
Its fields are\code{def}, the class definition, and \code{className}.

More importantly, generator objects have methods to generate objects
from the class, to access help on reference methods, and optionally to
define new reference methods for the class:
\describe{
\item{\code{$new(...)}}{ %$
This method is equivalent to the function \code{\link{new}} with the
class name as an argument.  The \dots arguments are values for the
named fields.
}
\item{\code{$help(topic)}}{ %$
Prints brief help on the topic.  The topics recognized
are reference method names, quoted or not.

The information printed is the calling sequence for the method, plus
self-documentation if any.
Reference methods can have an initial character string or vector as
the first element in the body of the function defining the method.
If so, this string is taken as self-documentation for the method (see
the section on Writing Reference Methods for details.

If no topic is given or if the topic is not a method name, the
definition of the class is printed.
}
\item{\code{$methods(...)}}{ %$
The named arguments should be method definitions, which will be
installed in the class, as if they had been supplied in the
\code{refMethods} argument to \code{setRefClass()}.

The new methods can refer to any currently defined method by name
(including other methods supplied in this call to
\code{$methods()}. %$
Note though that previously defined methods are not re-analyzed
meaning that they will not call the new method (unless it redefines an
existing method of the same name).

To remove a method, supply \code{NULL} as its new definition.
}
}
}

\section{Inter-System Interfaces}{
A number of \R packages currently support interfaces to other
languages using a similar reference-based programming model.
Aside from differences in choice of terminology and other details,
many of these are largely compatible with the programming style
described here.

The reference class definitions defined here provide a hook for
methods defined in the interface to be made available in \R.
The interface (typically, a package) implements a method for the
generic function \code{referenceMethods()} for the definition of the
class in the foreign language. In the \code{Rcpp} package for example,
an object of class \code{"C++Class"} is generated for each C++ class
exposed.  Depending on the information about the class exposed,
\R-callable versions of the C++ methods and of getter and setter
accessor methods for the properties may be made available.

The S4 method defined for \code{referenceMethods()} should return a named list of functions to be
called directly by the user (\code{x$f(...)}) %$
or from within class-based method definitions.
Calls to \code{referenceMethods()} will be made from
\code{setRefClass()} if argument \code{interfaceClasses} is a class
definition object (or a list of them), passing down the class
definition object.
The methods returned will be made available in the reference class.

Typically, if the designers of the inter-system interface want users
to be able to write \R methods using the same reference style, the
interface will include a call to \code{setRefClass()} including the
\code{interfaceClasses} argument.
If the interface wants to modify any of the methods (for example, to
use information not included in the class definition), it can do so by
invoking the \code{methods()} method on the generator object.
But all relevant methods should have some definition returned by
\code{referenceMethods()}, in order for the code analysis to recognize
that calls to them are method calls.

%%  For an example, see the code for package \code{Rcpp}, version
%%  0.8.7 or later.
}
\value{
\code{setRefClass} returns a generator object for the class. This is
itself a reference object, with methods to generate objects from the
class and also for defining new methods and for help-style
documentation. See the
section on generator objects for details.

\code{refClassFields} returns a list of the classes for all the class
fields.

\code{refClassMethods} returns a list of the class method functions,
including those inherited and accessor methods generated.

}
\section{NOTE:}{
The software described here remains under development.  The current implementation (\R
version 2.12.0) is preliminary and subject to change.
Developers of inter-system interface software for which the reference
class model is appropriate are particularly encouraged to experiment
with \R reference classes.
}
\author{
John Chambers
}


 \examples{
## a simple editor for matrix objects.  Method  edit() changes some
## range of values; method undo() undoes the last edit.
mEditor <- setRefClass("matrixEditor",
      fields = list( data = "matrix",
        edits = "list"),
      methods = list(
     edit = function(i, j, value) {
         backup <-
             list(i, j, data[i,j])
         edits <<- c(list(backup),
                     edits)
         data[i,j] <<- value
         invisible(value)
     },
     undo = function() {
         prev <- edits
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
     ))
    
xMat <- matrix(1:12,4,3)
xx <- mEditor$new(data = xMat)
xx$edit(2, 2, 0)
xx$data
xx$undo()
mEditor$help("undo")
stopifnot(all.equal(xx$data, xMat))
\dontshow{
tf <- tempfile()
xx$save(tf) #$
load(tf)
unlink(tf)
stopifnot(identical(xx$data, .self$data))
}

## Inheriting a reference class:  a matrix viewer
mv <- setRefClass("matrixViewer", contains = "matrixEditor",
    methods = list( view = function() matplot(data),
        edit = # invoke previous method, then replot
          function(i, j, value) {
            callSuper(i, j, value)
            view()
          }))

\dontshow{
ff = new("matrixViewer", data = xMat)
ff$edit(2,2,0)
ff$data
ff$undo()
stopifnot(all.equal(ff$data, xMat))
removeClass("matrixEditor")
removeClass("matrixViewer")
}
}
\keyword{ programming }
\keyword{ classes }
