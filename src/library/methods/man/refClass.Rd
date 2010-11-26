\name{ReferenceClasses}
\alias{ReferenceClasses}
\alias{setRefClass}
\alias{getRefClass}
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
Objects With Fields Treated by Reference (OOP-style)
}
\description{
The software described here supports reference classes whose objects have
fields
accessed by reference in the style of \dQuote{OOP} languages such as Java and
C++.
Computations with these objects invoke methods on them and
extract or set their fields.
The field and method computations potentially modify the object.
All computations referring to the objects see the modifications, in contrast to
the usual functional programming model in \R.
Reference classes can be used to program in \R directly or in combination
with an interface to an OOP-style language, allowing \R-written methods to
extend the interface.
}
\usage{
setRefClass(Class, fields = , contains = , methods =,
     where =, ...)

getRefClass(Class, where =)

}
\arguments{
  \item{Class}{
character string name for the class.

In the call to \code{getRefClass()} this argument can also be any
object from the relevant class;  note also the corresponding reference
class methods documented in the section on \dQuote{Writing Reference Methods}.
}
  \item{fields}{
either a character vector of field names or
a named list of the fields.  The resulting fields will be accessed with reference semantics (see
the  section on \dQuote{Reference Objects}).  If the argument is a list, the
elements of the list can be the character string name of a class, in
which case the field must be from that class or a subclass.

 The element in the list can alternatively be an \emph{accessor
   function}, a function of one argument that returns
the field if called with no argument or sets it to the value of the
argument otherwise.
Accessor functions are used internally and for inter-system interface
applications.
Their definition follows the rules for writing methods for the class:
they can refer to other fields and can call other methods for this
class or its superclasses.
See the section on \dQuote{Implementation} for the internal mechanism
used by accessor functions.

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

Two optional method names are interpreted specially, \code{initialize}
and \code{finalize}. If an \code{initialize} method is defined, it
will be invoked when an object is generated from the class.  See the
discussion of method \code{$new(...)} %$
in the section \dQuote{Reference Object Generators}.

If a \code{finalize} method is defined, a function will be
\link[=reg.finalizer]{registered} to invoke it before the environment
in the object is discarded by the garbage collector. See the matrix
viewer example for both initialize and finalize methods.


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
The corresponding programming mechanism
is to invoke a method on an object.
In the \R syntax that we use for this operation, one invokes a method,
\code{m1} say, on an object \code{x} by the expression
\code{x$m1(...)}. %$

Methods in this paradigm are associated with the object, or more
precisely with the class of the object, as opposed to methods in a
function-based class/method system, which are fundamentally associated
with the function (in \R, for example, a generic function in an \R
session has a table of all its currently known methods).
In this document \dQuote{methods for a class} as opposed to
\dQuote{methods for a function} will make the distinction.

Objects in this paradigm usually have named fields on which
the methods operate.
In the \R implementation, the fields are defined when the class is
created.
The field itself can optionally have a specified class, meaning that only objects
from this class or one of its subclasses can be assigned to the field.
By default, fields have class \code{"ANY"}.
Fields may also be defined by supplying an accessor function which
will be called to get or set the field.
Accessor functions are likely when reference classes are part of an
inter-system interface.
The interface will usually supply the accessor functions automatically
based on the definition of the corresponding class in the other language.

Fields are accessed by reference.
In particular, invoking a method may modify the content of
the fields.

Programming for such classes involves writing new methods for a
particular class.
In the \R implementation, these methods are \R functions, with zero or
more formal arguments.
The object on which the methods are invoked is not an explicit
argument to the method.
Instead, fields and methods for the class can be referred to by name
in the method definition.
The implementation uses \R environments to make fields and methods
available by name.
Additional special fields allow reference to the complete object and
to the definition of the class.  See the section on \dQuote{Inheritance}.

The goal of the software described here is to provide a uniform
programming style in \R for software dealing with reference classes, whether
implemented directly in \R or through an interface to one of the OOP
languages.
}


\section{Writing Reference Methods}{
Reference methods are functions supplied as elements of a named list,
either
when invoking \code{g$methods()} %$}
on a generator object \code{g} or as
the argument \code{methods} in a call to \code{setRefClass}.
They are written as ordinary \R functions but have some special
features and restrictions.
The body of the function can contain calls to any other reference method,
including those inherited from other reference classes and may refer
to fields in the object by name.

Fields may be modified in a method by using the
non-local assignment operator, \code{<<-}, as in the \code{$edit} and \code{$undo}
methods in the example below.
Note that non-local assignment is required:  a local assignment with
the \code{<-} operator just creates a local object in the function
call, as it would in any \R function.
When methods are installed, a heuristic check is made for local
assignments to field names and a warning issued if any are detected.

Reference methods should be kept simple; if they need to do some
specialized \R computation, that computation should use a separate \R
function that is called from the reference method.
Specifically, methods can not use special features of the
enclosing environment mechanism, since the method's environment is
used to access fields and other methods.
Reference methods can not themselves be generic functions; if you want
additional function-based method dispatch, write a separate generic
function and call that from the method.

The entire object can be referred to in a method by the reserved
name \code{.self}, as shown in the \code{save=} method of the
example.
The special object \code{.refClassDef} contains the definition of the
class of the object.

The methods available include methods inherited from superclasses, as
discussed in the next section.

Documentation for the methods can be obtained by the \code{$help} %$}
method for the generator object.
Methods for classes are not documented in the \code{Rd} format used
for \R functions.
Instead, the \code{$help} %$}
method prints the calling sequence of the method, followed by
self-documentation from the method definition, in the style of Python.
If the first element of the body of the method is a literal character
string (possibly multi-line), that string is interpreted as documentation.
See the method definitions in the example.
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

Class fields are inherited.  A class definition can override a field
of the same name in a superclass only if the overriding class is a
subclass of the class of the inherited field.  This ensures that a
valid object in the field remains valid for the superclass as well.

Inherited methods are installed in the same way as directly
specified methods.
The code in a method can refer to  inherited methods in the same
way as directly specified methods.

A method may override a method of the same name in a superclass.
The overriding method can call the superclass method by 
\code{callSuper(...)} as described below.

All reference classes inheit from the class \code{"envRefClass"},
including the following methods.

\describe{
\item{\code{$callSuper(...)}}{ %$
Calls the method inherited from a reference superclass.
The call is meaningful only from within another method, and will be
resolved to call the inherited method of the same name.
The arguments to \code{$callSuper} %$}
are passed to the superclass version.
See the matrix viewer class in the example.

Note that the intended arguments for the superclass method must be
supplied explicitly; there is no convention for supplying the
arguments automatically, in contrast to the similar mechanism for
functional methods.
}

\item{\code{$copy(shallow = FALSE)}}{ %$
Creates a copy of the object.  With reference classes, unlike ordinary
\R objects, merely assigning the object with a different name does not
create an independent copy.  If \code{shallow} is \code{FALSE}, any
field that is itself a reference object will also be copied, and
similarly recursively for its fields.  Otherwise, while reassigning a
field to a new reference object will have no side effect, modifying
such a field will still be reflected in both copies of the object.
The argument has no effect on non-reference objects in fields.  When
there are reference objects in some fields but it is asserted that
they will not be modified, using \code{shallow = TRUE} will save some
memory and time.
}

\item{\code{$getRefClass()}; \code{$getClass()}}{
These return respectively the generator object and the formal class
definition for the reference class of this object, efficiently.
}



\item{\code{$export(Class)}}{ %$
Returns the result of coercing the object to \code{Class} (typically
one of the superclasses of the object's class).  Calling the method
has no side effect on the object itself.
}

\item{\code{$import(value, Class = class(value))}}{ %$
Import the object \code{value} into the current object, replacing the
corresponding fields in the current object.
Object \code{value} must come from one of the superclasses of the
current object's class.
If argument \code{Class} is supplied, \code{value} is first coerced to
that class.
}

\item{\code{$initFields(...)}}{ %$
Initialize the fields of the object from the supplied arguments.  This
method is usually only called from a class with a \code{$initialize()}% $
method.  It corresponds to the default initialization for reference
classes.  If there are slots and non-reference superclasses, these may
be supplied in the \dots argument as well.

Typically, a specialized \code{$initialize()}% $
method carries out its own computations, then invokes \code{$initFields()}% $
to perform standard initialization, as shown in the
\code{matrixViewer} class in the example below.
}
} % end describe

Objects also inherit two reserved fields:
\describe{
\item{\code{.self}}{
a reference to the entire object;
}
\item{\code{.refClassDef}}{
the class definition.
}
} % end \describe
The defined fields should not override these, and in general it is
unwise to define a field whose name begins with \code{"."}, since the
implementation may use such names for special purposes.

}

\section{Reference Class Generator Objects}{
The call to \code{setRefClass} defines the specified class and
returns a \dQuote{generator} object for that class.
The generator object is itself a reference object (of class
\code{"refObjectGenerator"}).
Its fields are \code{def}, the class definition, and \code{className},
the character string name of the class.

Methods for generator objects exist to generate objects
from the class, to access help on reference methods, and to
define new reference methods for the class.
The currently available methods are:
\describe{
\item{\code{$new(...)}}{ %$
This method is equivalent to the function \code{\link{new}} with the
class name as an argument.  The \dots arguments are values for the
named fields.
If the class has a method defined for \code{$initialize()}, %$
this method will be called once the reference object has been
created.  You should write such a method for a class that needs to do
some special initialization.
In particular, a reference method is recommended rather than a method
for the S4 generic function, because some special initialization is
required for reference objects \emph{before} the initialization of
fields.
As with S4 classes, methods are written for \code{$initialize()} %$
and not for \code{$new()}, %$
both for the previous reason and also because \code{$new()} %$
is invoked on the generator object and would be a method for that class.

The default method for \code{$initialize()} %$
is equivalent to invoking the method \code{$initFields(...)} %$
with named fields as its arguments.  For technical reasons, the
default method does not currently appear explicitly, but can be
invoked by $callSuper(...) %$
from a method for \code{$initialize()}. %$
Initialization methods need some care in design, as they do for S4
classes.
In particular, remember that others may subclass your class and pass
through field assignments or other arguments.  Therefore, your method
should normally include \dots as an argument, all other arguments
should have defaults or check for missingness, and your method should
pass all initialized values on via \code{$callSuper()} or \code{$initFields()} if
you know that your superclasses have no initialization methods.
}

\item{\code{$help(topic)}}{ %$
Prints brief help on the topic.  The topics recognized
are reference method names, quoted or not.

The information printed is the calling sequence for the method, plus
self-documentation if any.
Reference methods can have an initial character string or vector as
the first element in the body of the function defining the method.
If so, this string is taken as self-documentation for the method (see
the section on \dQuote{Writing Reference Methods} for details).

If no topic is given or if the topic is not a method name, the
definition of the class is printed.
}
\item{\code{$methods(...)}}{ %$
With no arguments, returns a list of the reference methods for this
class.

Named arguments
are method definitions, which will be
installed in the class, as if they had been supplied in the
\code{methods} argument to \code{setRefClass()}.

The new methods can refer to any currently defined method by name
(including other methods supplied in this call to
\code{$methods()}. %$
Note though that previously defined methods are not re-analyzed
meaning that they will not call the new method (unless it redefines an
existing method of the same name).

To remove a method, supply \code{NULL} as its new definition.
}

\item{\code{$fields()}}{ %$}{
Returns a list of the fields, each with its corresponding class.
Fields for which an accessor function was supplied in the definition
have class \code{"activeBindingFunction"}.
}

\item{\code{$lock(...)}}{ %$}{
The fields named in the arguments are locked; specifically, after the
lock method is called, the field may be set once.  Any further attempt
to set it will generate an error.

Fields that are defined by an explicit accessor function can not be
locked (on the other hand, the accessor function can be defined to
generate an error if called with an argument).
}

\item{\code{$accessors(...)}}{ %$}{
A number of
systems using the OOP programming paradigm recommend or enforce
\emph{getter and setter methods}
corresponding to each field, rather than direct access by name.
In the \R version presented here (and fairly often elsewhere
as well), a field named \code{abc} of an object \code{x} would be
extracted by \code{x$getAbc()} and assigned by
\code{x$setAbc(value)}.
The \code{$accessors} %$}
method is a convenience function that creates getter and setter methods for the
specified fields.
}
} %% end of \describe
} %% end of \section

\section{Implementation}{
Reference classes are implemented as S4 classes with a data part of
type \code{"environment"}.
An object generated from a reference class has this type.
Fields correspond to named objects in the environment.
A field associated with an accessor function is implemented as an
\link[=bindenv]{active binding}.
In addition, fields with a specified class are implemented as a
special form of active binding to enforce valid assignment to the
field.
A field, say \code{data}, can be accessed generally by an expression
of the form \code{x$data} %$}
for any object from the relevant class.
In a method for this class, the field can be accessed by the name
\code{data}.
A field that is not locked can be set by an expression of the form
\code{x$data <- value}.%$
Inside a method, a field can be assigned by an expresion of the form
\code{x <<- value}.
Note the \link[=assignOps]{non-local assignment} operator.
The standard \R interpretation of this operator works to assign it in
the environment of the object.
If the field has an accessor function defined, getting and setting
will call that function.

When a method is invoked on an object, the function defining the method is
installed in the object's environment, with the same environment as the
environment of the function.

}

\section{Inter-System Interfaces}{
A number of 
languages use a similar reference-based programming model with classes
and class-based methods.
Aside from differences in choice of terminology and other details,
many of these languages are compatible with the programming style
described here.
\R interfaces to the languages exist in a number of packages.

The reference class definitions here provide a hook for
methods defined in the interface to used and extended in \R.
Access to fields and/or methods in the foreign language can be
implemented by defining an \R reference class corresponding to
classes made available through the interface.
Typically, the inter-system interface will take care of the details of
such methods, given a description of the foreign class (what fields
and methods it has, the classes for the fields, whether any are
read-only, etc.)
The specifics for the fields and methods can be implemented via
reference methods for the \R class.
In particular, the use of active bindings allows field access for
getting and setting, with
actual access handled by the inter-system interface.

\R methods and/or fields can be included in the class definition as for any
reference class.

For an inter-system interface using this approach, see the code for package \code{Rcpp}, version
0.8.7 or later.
}
\value{
\code{setRefClass} and \code{getRefClass} both return a generator object for the class. This is
itself a reference object, with methods to generate objects from the
class and also for defining new methods and for help-style
documentation. See the
section on \dQuote{Reference Class Generator Objects} for details.
Note that \code{Class} in the call to \code{getRefClass()} can be an
object from the corresponding class, and that a similar reference
class method \code{$getRefClass()} %$
is available as well.

\code{setRefClass} defines the class and stores its class definition.
\code{getRefClass} requires that the class has been defined as a
reference class.

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
## a simple editor for matrix objects.  Method  $edit() changes some
## range of values; method $undo() undoes the last edit.
mEditor <- setRefClass("matrixEditor",
      fields = list( data = "matrix",
        edits = "list"),
      methods = list(
     edit = function(i, j, value) {
       ## the following string documents the edit method
       'Replaces the range [i, j] of the
        object by value.
        '
         backup <-
             list(i, j, data[i,j])
         data[i,j] <<- value
         edits <<- c(list(backup),
                     edits)
         invisible(value)
     },
     undo = function() {
       'Undoes the last edit() operation
        and update the edits field accordingly.
        '
         prev <- edits
         if(length(prev)) prev <- prev[[1]]
         else stop("No more edits to undo")
         edit(prev[[1]], prev[[2]], prev[[3]])
         ## trim the edits list
         length(edits) <<- length(edits) - 2
         invisible(prev)
     }
     ))
    
xMat <- matrix(1:12,4,3)
xx <- mEditor$new(data = xMat)
xx$edit(2, 2, 0)
xx$data
xx$undo()
mEditor$help("undo")
stopifnot(all.equal(xx$data, xMat))

## add a method to save the object
mEditor$methods(
     save = function(file) {
       'Save the current object on the file
        in R external object format.
       '
         base::save(.self, file = file)
     }
)  

tf <- tempfile()
xx$save(tf) #$
\dontshow{
load(tf)
unlink(tf)
stopifnot(identical(xx$data, .self$data))
}

\dontrun{
## Inheriting a reference class:  a matrix viewer
mv <- setRefClass("matrixViewer", 
    fields = c("viewerDevice", "viewerFile"),
    contains = "matrixEditor",
    methods = list( view = function() {
        dd <- dev.cur(); dev.set(viewerDevice)
        devAskNewPage(FALSE)
        matplot(data, main = paste("After",length(edits),"edits"))
        dev.set(dd)},
        edit = # invoke previous method, then replot
          function(i, j, value) {
            callSuper(i, j, value)
            view()
          }))

## initialize and finalize methods
mv$methods( initialize = 
  function(file = "./matrixView.pdf", ...) {
    viewerFile <<- file
    pdf(viewerFile)
    viewerDevice <<- dev.cur()
    dev.set(dev.prev())
    callSuper(...)
  },
  finalize = function() {
    dev.off(viewerDevice)
  })
}
\dontshow{
removeClass("matrixEditor")
resetGeneric("$")
resetGeneric("initialize")
} %$
}
\keyword{ programming }
\keyword{ classes }
