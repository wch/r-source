% File src/library/methods/man/refClass.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2010-2014 R Core Team
% Distributed under GPL 2 or later

\name{ReferenceClasses}
\title{Objects With Fields Treated by Reference (OOP-style)}
\alias{ReferenceClasses}
\alias{setRefClass}
\alias{getRefClass}
\alias{initFieldArgs}
\alias{initRefFields}
\alias{activeBindingFunction-class}
\alias{defaultBindingFunction-class}
\alias{uninitializedField-class}
\alias{refClassRepresentation-class}
\alias{refObjectGenerator-class}
\alias{refGeneratorSlot-class}
\alias{refClass-class}
\alias{refObject-class}
\alias{refMethodDef-class}
\alias{refMethodDefWithTrace-class}
\alias{SuperClassMethod-class}
\alias{show,envRefClass-method}
\alias{show,refMethodDef-method}
\alias{show,externalRefMethod-method}
\alias{show,refClassRepresentation-method}
\alias{externalRefMethod}
\alias{externalRefMethod-class}
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
     where =, inheritPackage =, ...)

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
standard \R{} object management.
It is not generally a good idea to mix slots and fields in the same
class; this confuses the distinction in behavior between reference
classes and regular S4 classes.  See the comments in the
\dQuote{Implementation} section.
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
viewer example for both initialize and finalize methods.}

  \item{where}{
the environment in which to store the class definition (or to begin the
search for it, in
the case of \code{getRefClass()}).  Defaults to
the package namespace or environment for code that is part of an \R{}
package, and to the global environment for code sourced directly at
the session top level.
}
  \item{inheritPackage}{
Should objects from the new class inherit the package environment of a
contained superclass?  Default \code{FALSE}.  See the Section \dQuote{Inter-Package Superclasses
  and External Methods}.
} 
  \item{\dots}{
other arguments to be passed to \code{\link{setClass}}.
}
}

\value{
  \code{setRefClass()} returns a generator function suitable for
  creating objects from the class, invisibly.  A call to this function
  takes any number of arguments,
  which will be passed on to the initialize method.  If no
  \code{initialize} method is defined for the class or one of its
  superclasses, the default method expects named arguments with the
  name of one of the fields and unnamed arguments, if any, that are
  objects from one of the superclasses of this class (but only
  superclasses that are themselves reference classes have any effect).

  The generator function is similar to the S4 generator function
  returned by \code{\link{setClass}}. In addition to being a generator
  function, however, it is also a reference class generator object,
  with reference class methods for various utilities.  See the section
  on reference class generator objects below.

If the class has a method defined for \code{$initialize()}, %$
this method will be called once the reference object has been
created.  You should write such a method for a class that needs to do
some special initialization.
In particular, a reference method is recommended rather than a method
for the S4 generic function \code{initialize()}, because some special initialization is
required for reference objects \emph{before} the initialization of
fields.
As with S4 classes, methods are written for \code{$initialize()} %$
and not for \code{$new()}, %$
both for the previous reason and also because \code{$new()} %$
is invoked on the generator object and would be a method for that class.

The default method for \code{$initialize()} %$
is equivalent to invoking the method \code{$initFields(...)}. %$
Named arguments assign initial values to the corresponding fields.
Unnamed arguments must be objects from this class or a reference
superclass of this class.
Fields will be initialized to the contents of the fields in such
objects, but named arguments override the corresponding inherited
fields.
Note that fields are simply assigned.  If the field is itself a
reference object, that object is not copied.
The new and previous object will share the reference.
Also, a field assigned from an unnamed argument counts as an
assignment for locked fields.
To override an inherited value for a locked field, the new value must
be one of the named arguments in the initializing call.
A later assignment of the field will result in an error.

Initialization methods need some care in design.
The generator
for a reference class will be called with no arguments, for example
when copying the object.
To ensure that these calls do not fail, the method must have defaults
for all arguments or check for \code{missing()}.
The method
should include \code{\dots} as an argument and
pass this on via \code{$callSuper()} (or \code{$initFields()} if
you know that your superclasses have no initialization methods).
This allows future class definitions that subclass this class, with
additional fields.

\code{getRefClass()} also returns the generator function for the
class.  Note that the package slot in the value is the correct package
from the class definition, regardless of  the \code{where} argument,
which is used only to
find the class if necessary.
}


\section{Reference Objects}{
Normal objects in \R are passed as arguments in function calls consistently with
functional programming semantics; that is, changes made to an object
passed as an argument are local to the function call.  The object that
supplied the argument is unchanged.

The functional model (sometimes called pass-by-value, although this is
inaccurate for \R) is
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
In the \R syntax we use \code{"$"} %$
for this operation; one invokes a method,
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
to the definition of the class.  See the section on \dQuote{Writing
  Reference Methods}.

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

Alternatively, a method may be an \emph{external} method, a function
whose first argument is \code{.self}.
The body of such methods works like any ordinary function.
The methods are called like other methods (without the \code{.self}
argument, which is supplied internally and always refers to the object
itself).
External methods exist so that reference classes can inherit the
package environment of superclasses
in other packages; see the Section \dQuote{Inter-Package Superclasses
  and External Methods}.

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
In particular, methods should not use non-exported entries in the
package's namespace, because the methods may be inherited by a
reference class in another package.

Methods for \code{$initialize()} %$
have special requirements.  See the comments in the \dQuote{Value}
section.

Reference methods can not themselves be generic functions; if you want
additional function-based method dispatch, write a separate generic
function and call that from the method.

The entire object can be referred to in a method by the reserved
name \code{.self}, as shown in the \code{save=} method of the
example.
The special object \code{.refClassDef} contains the definition of the
class of the object.
These fields are read-only (it makes no sense to modify these
references), with one exception.
In principal, the \code{.self} field can be modified in the \code{$initialize} %$
method, because the object is still being created at this stage.
This is definitely not recommended, unless to set some
non-reference properties of the object defined for this class, which
is itself not recommended if it mixes slots and fields.


The methods available include methods inherited from superclasses, as
discussed in the next section.

Only methods actually used will be included in the environment
corresponding to an individual object.  To declare that a method requires a
particular other method, the first method should include a call
to \code{$usingMethods()} %$
with the name of the other method as an argument.
Declaring the methods this way is essential if the other method is used indirectly (e.g., via \code{\link{sapply}()}
or \code{\link{do.call}()}).
If it is called directly, code analysis will find it.
Declaring the method is harmless in any case, however, and may aid
readability of the source code.

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
The names of the reference superclasses are in slot
\code{refSuperClasses} of the class definition.
Reference classes can inherit from ordinary S4 classes also, but this
is usually a bad idea if it mixes reference fields and non-reference slots.
See the comments in the section on \dQuote{Implementation}.

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

All reference classes inherit from the class \code{"envRefClass"},
which provides the following methods.

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

\item{\code{$field(name, value)}}{ %$
With one argument, returns the field of the object with character
string \code{name}.  With two arguments, the corresponding field is
assigned \code{value}.  Assignment checks that \code{name} specifies a
valid field, but the single-argument version will attempt to get
anything of that name from the object's environment.

The \code{$field()} %$
method replaces the direct use of a field name, when the name of the
field must be calculated, or for looping over several fields.
}

\item{\code{$export(Class)}}{ %$
Returns the result of coercing the object to \code{Class} (typically
one of the superclasses of the object's class).  Calling the method
has no side effect on the object itself.
}

\item{\code{$getRefClass()}; \code{$getClass()}}{
These return respectively the generator object and the formal class
definition for the reference class of this object, efficiently.
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

\item{\code{$show()}}{ %$
This method is called when the object is printed automatically,
analogously to the \code{\link{show}} function.  A general method is
defined for class \code{"envRefClass"}.  User-defined reference
classes will often define their own method: see the Example below.

Note two points in the example.  As with any \code{show()} method, it
is a good idea to print the class explicitly to allow for subclasses
using the method.  Second, to call the \emph{function} \code{show()}
from the method, as opposed to the \code{$show()} %$
method itself, refer to \code{methods::show()} explicitly.
}

\item{\code{$trace(what, ...)}, \code{$untrace(what)} }{
Apply the tracing and debugging facilities of the \code{\link{trace}}
function to the reference method \code{what}.

All the arguments of the \code{\link{trace}}
function can be supplied, except for \code{signature}, which is not
meaningful.

The reference method can be invoked on either an object or the
generator for the class.  See the section on Debugging below for details.
}

\item{\code{$usingMethods(...)}}{ %$
Reference methods used by this method are named as the arguments
 either quoted or unquoted.  In the code analysis phase of installing the
 the present method, the declared methods will be included.  It is essential
 to declare any methods used in a nonstandard way (e.g., via an apply function).
 Methods called directly do not need to be declared, but it is harmless to do so.
 \code{$usingMethods()} does nothing at run time. %$
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

\section{Inter-Package Superclasses  and External Methods}{
The environment of a method in a reference class is the object itself,
as an environment.
This allows the method to refer directly to fields and other methods,
without using the whole object and the \code{"$"} %$
operator.
The parent of that environment is the namespace of the package in
which the reference class is defined.
Computations in the method have access to all the objects in the
package's namespace, exported or not.

When defining a class that contains a reference superclass in another
package, there is an ambiguity about which package namespace should
have that role.
The argument \code{inheritPackage} to \code{setRefClass()} controls
whether the environment of new objects should inherit from an
inherited class in another package or continue to inherit from the
current package's namespace.

If the superclass is \dQuote{lean}, with few methods, or exists
primarily to support a family of subclasses, then it may be better to
continue to use the new package's environment.
On the other hand, if the superclass was originally written as a
standalone, this choice may invalidate existing superclass methods.
For the superclass methods to continue to work, they must use only
exported functions in their package and the new package must import
these.

Either way, some methods may need to be written that do \emph{not}
assume the standard model for reference class methods, but behave
essentially as ordinary functions would in dealing with reference
class objects.

The mechanism is to recognize \emph{external methods}.
An external method  is
written as a function in which the first argument, named \code{.self},
stands for the reference class object.
This function is supplied as the definition for a reference class method.
The method will be called, automatically, with the first argument
being the current object and the other arguments, if any, passed along
from the actual call.

Since an external method is an ordinary function in the source code
for its package, it has access to all the objects in the namespace.
Fields and methods in the reference class must be referred to in the
form \code{.self$name}.%$

If for some reason you do not want to use \code{.self} as the first
argument, a function \code{f()} can be converted explicitly as
\code{externalRefMethod(f)}, which returns an object of class
\code{"externalRefMethod"} that can be supplied as a method for the
class.
The first argument will still correspond to the whole object.

External methods can be supplied for any reference class, but there is no
obvious advantage unless they are needed.
They are more work to write, harder to read and (slightly) slower to
execute.

}

\section{Reference Class Generators}{
The call to \code{setRefClass} defines the specified class and
returns a \dQuote{generator function} object for that class.
This object has class \code{"refObjectGenerator"}; it inherits
from \code{"function"} via \code{"classGeneratorFunction"} and can be
called to generate new objects from the reference class.

The returned object is also a reference class object, although not of
the standard construction.
It can be used to invoke reference methods and access fields in the usual way, but
instead of being implemented directly as an environment it has a
subsidiary generator object as a slot, a
standard reference object (of class
\code{"refGeneratorSlot"}).
Note that if one wanted to extend the reference class generator
capability with a subclass, this should be done by subclassing
\code{"refGeneratorSlot"}, not \code{"refObjectGenerator"}.

The fields are \code{def}, the class definition, and \code{className},
the character string name of the class.
Methods generate objects
from the class, to access help on reference methods, and to
define new reference methods for the class.
The currently available methods are:
\describe{
\item{\code{$new(...)}}{ %$
This method is equivalent to calling the generator function returned
by \code{setRefClass}.
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
With no arguments, returns the names of the reference methods for this
class.
With one character string argument, returns the method of that name.

Named arguments
are method definitions, which will be
installed in the class, as if they had been supplied in the
\code{methods} argument to \code{setRefClass()}.
Supplying methods in this way, rather than in the call to
\code{setRefClass()}, is recommended for the sake of clearer source
code.
See the section on \dQuote{Writing Reference Methods} for details.

All methods for a class should be defined in the source code that
defines the class, typically as part of a package.
In particular, methods can not be redefined in a class in an attached
package with a namespace: The class method checks for a locked
binding of the class definition.


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

If called with no arguments, the method returns the names of the
locked fields.

Fields that are defined by an explicit accessor function can not be
locked (on the other hand, the accessor function can be defined to
generate an error if called with an argument).

All code to lock fields should normally be part of the definition of a
class; that is, the read-only nature of the fields is meant to be part
of the class definition, not a dynamic property added later.
In particular, fields can not be locked in a class in an attached
package with a namespace:  The class method checks for a locked
binding of the class definition.  Locked fields can not be
subsequently unlocked.
}

\item{\code{$trace(what, ..., classMethod = FALSE)}}{ %$}{
Establish a traced version of method \code{what} for objects generated
from this class.  The generator object tracing works like the
\code{$trace()}%$
method for objects from the class, with two differences.
Since it changes the method definition in the class object itself,
tracing applies to all objects, not just the one on which the trace
method is invoked.

Second, the optional argument \code{classMethod = TRUE} allows tracing
on the methods of the generator object itself.
By default, \code{what} is interpreted as the name of a method in the
class for which this object is the generator.
}

\item{\code{$accessors(...)}}{ %$}{
A number of
systems using the OOP programming paradigm recommend or enforce
\emph{getter and setter methods}
corresponding to each field, rather than direct access by name.
If you like this style and want to  extract a field named \code{abc}
by \code{x$getAbc()} and assign it by
\code{x$setAbc(value)},
the \code{$accessors} %$}
method is a convenience function that creates such getter and setter methods for the
specified fields.
Otherwise there is no reason to use this mechanism.  In particular, it
has nothing to do with the general ability to define fields by
functions as described in the section on \dQuote{Reference Objects}.
}
} %% end of \describe
} %% end of \section

\section{Implementation}{
Reference classes are implemented as S4 classes with a data part of
type \code{"environment"}.
Fields correspond to named objects in the environment.
A field associated with a function is implemented as an
\link[=bindenv]{active binding}.
In particular, fields with a specified class are implemented as a
special form of active binding to enforce valid assignment to the
field.
A field, say \code{data}, can be accessed generally by an expression
of the form \code{x$data} %$}
for any object from the relevant class.
In a method for this class, the field can be accessed by the name
\code{data}.
A field that is not locked can be set by an expression of the form
\code{x$data <- value}.%$
Inside a method, a field can be assigned by an expression of the form
\code{x <<- value}.
Note the \link[=assignOps]{non-local assignment} operator.
The standard \R interpretation of this operator works to assign it in
the environment of the object.
If the field has an accessor function defined, getting and setting
will call that function.

When a method is invoked on an object, the function defining the method is
installed in the object's environment, with the same environment as the
environment of the function.

Because of the implementation, new reference classes can inherit from
non-reference S4 classes as well as reference classes.
This is usually a bad idea, if the slots from the non-reference
class are thought of as alternatives to fields.
Unless there is some special argument in favor, mixing the functional
and reference paradigms for properties of the same object is
conceptually unclear.
In addition, the initialization method for the class will have to sort
out fields from slots, with a good chance of creating anomalous
behavior for subclasses of this class.
Better in general to define fields analogous to the slots in the S4
class, and to initialize those from an S4 object of that class.

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
classes in the foreign language to be exposed in \R.
Access to fields and/or methods in the class can be
implemented by defining an \R reference class corresponding to
classes made available through the interface.
Typically, the inter-system interface will take care of the details of
creating the \R class, given a description of the foreign class (what fields
and methods it has, the classes for the fields, whether any are
read-only, etc.)
The specifics for the fields and methods can be implemented via
reference methods for the \R class.
In particular, the use of active bindings allows field access for
getting and setting, with
actual access handled by the inter-system interface.

\R methods and/or fields can be included in the class definition as for any
reference class.
The methods can use or set fields and can call other methods transparently
whether the field or method comes from the interface or is defined
directly in \R.

For an inter-system interface using this approach, see the code for package \code{Rcpp}, version
0.8.7 or later.
}
% \value{
% \code{setRefClass()} and \code{getRefClass()} both return a generator object for the class. This is
% itself a reference object, with methods to generate objects from the
% class and also for defining new methods and for help-style
% documentation. See the
% section on \dQuote{Reference Class Generator Objects} for details.
% Note that \code{Class} in the call to \code{getRefClass()} can be an
% object from the corresponding class, and that a similar reference
% class method \code{$getRefClass()} %$
% is available as well.

% \code{setRefClass} defines the class and stores its class definition.
% \code{getRefClass} requires that the class has been defined as a
% reference class.

% }
\section{Debugging}{
The standard \R{} debugging and tracing facilities can be applied to
reference methods.
Reference methods can be passed to \code{\link{debug}} and its
relatives from an object to debug further method invocations on that
object; for example, \code{debug(xx$edit)}. %$

Somewhat more flexible use is available for a reference method version
of the \code{\link{trace}} function.
A corresponding \code{$trace()} %$
reference method is available for
either an object or for the reference class generator
(\code{xx$trace()} or \code{mEdit$trace()} in the example below).
Using \code{$trace()} on an object sets up a tracing
version for future invocations of the specified method for that
object.
Using \code{$trace()} on the generator for the class sets up a
tracing version for all future objects from that class (and sometimes for
existing objects from the class if the method is not declared or
previously invoked).

In either case, all the arguments to the standard  \code{\link{trace}}
function are available, except for \code{signature=} which is
meaningless since reference methods can not be S4 generic functions.
This includes the typical style \code{trace(what, browser)} for
interactive debugging and  \code{trace(what, edit = TRUE)} to edit the
reference method interactively.

}
\author{
  John Chambers
}

\examples{
## a simple editor for matrix objects.  Method  $edit() changes some
## range of values; method $undo() undoes the last edit.
mEdit <- setRefClass("mEdit",
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
         edits <<- c(edits, list(backup))
         invisible(value)
     },
     undo = function() {
       'Undoes the last edit() operation
        and update the edits field accordingly.
        '
         prev <- edits
         if(length(prev)) prev <- prev[[length(prev)]]
         else stop("No more edits to undo")
         edit(prev[[1]], prev[[2]], prev[[3]])
         ## trim the edits list
         length(edits) <<- length(edits) - 2
         invisible(prev)
     },
     show = function() {
       'Method for automatically printing matrix editors'
       cat("Reference matrix editor object of class",
          classLabel(class(.self)), "\n")
       cat("Data: \n")
       methods::show(data)
       cat("Undo list is of length", length(edits), "\n")
     }
     ))

xMat <- matrix(1:12,4,3)
xx <- mEdit(data = xMat)
xx$edit(2, 2, 0)
xx
xx$undo()
mEdit$help("undo")
stopifnot(all.equal(xx$data, xMat))

utils::str(xx) # show fields and names of non-trivial methods

## add a method to save the object
mEdit$methods(
     save = function(file) {
       'Save the current object on the file
        in R external object format.
       '
         base::save(.self, file = file)
     }
)

tf <- tempfile()
xx$save(tf)
\dontshow{
load(tf)
unlink(tf)
stopifnot(identical(xx$data, .self$data))
}

\dontrun{
## Inheriting a reference class:  a matrix viewer
mv <- setRefClass("matrixViewer",
    fields = c("viewerDevice", "viewerFile"),
    contains = "mEdit",
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

## debugging an object: call browser() in method $edit()
xx$trace(edit, browser)

## debugging all objects from class mEdit in method $undo()
mEdit$trace(undo, browser)
}
\dontshow{
removeClass("mEdit")
resetGeneric("$")
resetGeneric("initialize")
} %$
}
\keyword{ programming }
\keyword{ classes }
