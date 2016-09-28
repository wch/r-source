% File src/library/methods/man/Methods.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2016 R Core Team
% Distributed under GPL 2 or later

\name{Methods_Details}
\alias{Methods_Details}
\title{General Information on Methods}
\description{
  This documentation covers some general topics on how methods
  work and how the \pkg{methods} package interacts with the rest of \R.  The
  information is usually not needed to get started with methods and
  classes, but may be helpful for moderately ambitious projects, or when
  something doesn't work as expected.

  For additional information  see documentation for
  the important steps: (\code{\link{setMethod}()},
  \code{\link{setClass}()} and \code{\link{setGeneric}()}). Also
  \code{\link{Methods_for_Nongenerics}} on defining formal methods for
  functions  that are not currently generic functions;
  \link{Methods_for_S3} for the relation to S3 classes and methods;
  \code{\link{Classes_Details}} for class definitions and
  Chapters 9 and 10 of the reference.
}

\section{How Methods Work}{
  A call to a generic function selects a method matching the actual
  arguments in the call. The body of the method is evaluated in the
  frame of the call to the generic function.
  A generic function is identified by its name and by the package to
  which it correspond.  Unlike ordinary functions, the generic has a
  slot that specifies its package.

  In an \R session, there is one version of each such generic,
  regardless of where the call to that generic originated, and
  the generic function has a table of all the methods currently
  available for it; that is, all the methods
  in packages currently loaded into the session.

  Methods are frequently defined for functions that are non-generic in
  their original package,.
  for example, for function \code{plot()} in
  package \pkg{graphics}.
  An identical version of the corresponding generic function may exist
  in several packages.  All methods will be dispatched consistently
  from the \R session.

  Each \R package with a call to \code{\link{setMethod}} in its source code
  will include  a methods metadata object for that generic.
  When the package is loaded into an \R session, the methods for each
  generic function are \emph{cached}, that is, added to the
  environment of the generic function.  This merged table of methods is used to
  dispatch or select methods from the generic, using class inheritance
  and possibly group generic functions (see
  \code{\link{GroupGenericFunctions}}) to find an applicable method.
  See the \dQuote{Method Selection and Dispatch} section below.
  The caching computations ensure that only one version of each
  generic function is visible globally; although different attached
  packages may contain a copy of the generic function, these behave
  identically with respect to method selection.

  In contrast, it is possible for the same function name to refer to
  more than one generic function, when these have different
  \code{package} slots.  In the latter case, \R considers the
  functions unrelated:  A generic function is defined by the
  combination of name and package.  See the \dQuote{Generic Functions}
  section below.

  The methods for a generic are stored according to the
  corresponding \code{signature} in the call to \code{\link{setMethod}}
 that defined  the method.  The signature associates one
  class name with each of a subset of the formal arguments to the
  generic function.  Which formal arguments are available, and the
  order in which they appear, are determined by the \code{"signature"}
  slot of the generic function itself.  By default, the signature of the
  generic consists of all the formal arguments except \dots, in the
  order they appear in the function definition.

  Trailing arguments in the signature of the generic will be \emph{inactive}  if no
  method has yet been specified that included those arguments in its signature.
  Inactive arguments are not needed or used in labeling the cached
  methods.  (The distinction does not change which methods are
  dispatched, but ignoring inactive arguments improves the
  efficiency of dispatch.)

  All arguments in the signature of the generic function will be evaluated when the
  function is called, rather than using lazy
  evaluation.  Therefore, it's important to \emph{exclude}
  from the signature any arguments that need to be dealt with
  symbolically (such as the \code{expr} argument to function
  \code{\link{with}}).  Note that only actual arguments are
  evaluated, not default expressions.
  A missing argument enters into the method selection as class
  \code{"missing"}.

  The cached methods are stored in an
  environment object.  The names used for assignment are a
  concatenation of the class names for the active arguments in the method signature.

}
\section{Method Selection: Details}{

When a call to a generic function is evaluated, a method is selected corresponding
to the classes of the actual arguments in the signature.
First, the cached methods table is searched for an  exact match;
that is, a method stored under the signature defined by
the string value of \code{class(x)} for each non-missing
argument, and \code{"missing"} for each missing argument.
If no method is found directly for the actual arguments in a call to a
generic function, an attempt is made to match the available methods to
the arguments by using the superclass information about the actual
classes.
A method found by this search is cached
in the generic function so that future calls with the same argument classes will
not require repeating the search.  In any likely application, the
search for inherited methods will be a negligible overhead.

Each class definition may include a list of  one or more direct
\emph{superclasses} of the new class.
The simplest and most common specification is by the \code{contains=} argument in
the  call to \code{\link{setClass}}.
Each class named in this argument is a superclass of the new class.
A class will also have as a direct superclass any class union to which
it is a member.
Class unions are created by
a call to \code{\link{setClassUnion}}.
Additional members can be added to the union by a simple call to
\code{\link{setIs}}.
Superclasses specified by either mechanism are the \emph{direct} superclasses.


Inheritance specified in either of these forms is \emph{simple} in the
sense that all the information needed for the superclass is asserted
to be directly available from the object.
\R inherited from S a more general form of inheritance in which
inheritance may require some transformation or be conditional on a
test.
This more general form has not proved to be useful in general
practical situations.   Since it also adds some computational costs
non-simple inheritance is not recommended.  See \code{\link{setIs}}
for the general version.

The direct superclasses themselves may
have  direct superclasses and
similarly through further generations.  Putting all this information together produces
the full list of superclasses for this class.
The superclass list is included in the definition of the class that is
cached during the \R session.
The \emph{distance} between the two classes is defined to be the
number of generations:
\code{1} for direct superclasses (regardless of which mechanism
defined them), then \code{2} for the direct superclasses of those
classes, and so on.
To see all the superclasses, with their distance, print the class
definition by calling \code{\link{getClass}}.
In addition, any class implicitly has class \code{"ANY"} as a superclass.  The
distance to \code{"ANY"} is treated as larger than the distance to any
actual class.
The special class \code{"missing"} corresponding to missing arguments
has only \code{"ANY"} as a superclass, while \code{"ANY"} has no
superclasses.

When a method is to be selected by inheritance, a search is made in
the table for all methods corresponding to a combination of
either the direct class or one of its superclasses, for each argument
in the active signature.
For an example, suppose there is only one argument in the signature and that the class of
the corresponding object was \code{"dgeMatrix"} (from the recommended package
\code{Matrix}).
This class has (currently) three direct superclasses and through these
additional superclasses at distances 2 through 4.
A method that had been defined for any of these classes or for class
\code{"ANY"} (the default method) would be eligible.
Methods for the shortest difference are preferred.
If there is only one best method in this sense, method selection is unambiguous.

When there are multiple arguments in the signature, each argument will
generate a similar  list of inherited classes.
The possible matches are now all the combinations of classes from each
argument (think of the function \code{outer} generating an array of
all possible combinations).
The search now finds all the methods matching any of this combination
of classes.
For each argument, the distance to the superclass defines which
method(s) are preferred for that argument.
A method is considered best for selection if it is among the best
(i.e., has the least distance) for
each argument.

The end result is that zero, one or more methods may be \dQuote{best}.
If one, this method is selected and cached in the table of methods.
If there is more than one best match, the selection is ambiguous and a message is
printed noting which method was selected (the first method
lexicographically in the ordering) and what other methods could have
been selected.
Since the ambiguity is usually nothing the end user could control,
this is not a warning.
Package authors should examine their package for possible ambiguous
inheritance by calling \code{\link{testInheritedMethods}}.

Cached inherited selections are
not themselves used in future inheritance searches, since that could result
in invalid selections.
If you want inheritance computations to be done again (for example,
because a newly loaded package has a more direct method than one
that has already been used in this session), call
\code{\link{resetGeneric}}.  Because classes and methods involving
them tend to come from the same package, the current implementation
does not reset all generics every time a new package is loaded.

Besides being initiated through calls to the generic function, method
selection can be done explicitly by calling the function
\code{\link{selectMethod}}.
Note that some computations may use this function directly, with
optional arguments.
The prime example is the use of \code{\link{coerce}()} methods by
function \code{\link{as}()}.
There has been some confusion from comparing coerce methods to a call
to \code{\link{selectMethod}} with other options.
}

\section{Method Evaluation: Details}{

Once a method has been selected, the evaluator creates a new context
in which a call to the method is evaluated.
The context is initialized with the arguments from the call to the
generic function.
These arguments are not rematched.  All the arguments in the signature
of the generic will have been evaluated (including any that are
currently inactive); arguments that are not in the signature will obey
the usual lazy evaluation rules of the language.
If an argument was missing in the call, its default expression if any
will \emph{not} have been evaluated, since method dispatch always uses
class \code{missing} for such arguments.

A call to a generic function therefore has two contexts:  one for the
function and a second for the method.
The argument objects will be copied to the second context, but not any
local objects created in a nonstandard generic function.
The other important distinction is that the parent
(\dQuote{enclosing}) environment of the second context is the environment
of the method as a function, so that all \R programming techniques
using such environments apply to method definitions as ordinary functions.


For further discussion of method selection and dispatch,  see the
references in the sections indicated.

}

\section{Generic Functions}{
In principle, a generic function could be any function that evaluates
a call to \code{standardGeneric()}, the internal function that selects
a method and evaluates a call to  the selected method.  In practice,
generic functions are special objects that in addition to being from a
subclass of class \code{"function"} also extend the class
\code{\linkS4class{genericFunction}}.  Such objects have slots to define
information needed to deal with their methods.  They also have
specialized environments, containing the tables used in method
selection.

The slots \code{"generic"} and  \code{"package"} in the object are the
character string names of the generic function itself and of the
package from which the  function is defined.
As with classes, generic functions are uniquely defined in \R by the
combination of the two names.
There can be generic functions of the same name associated with
different packages (although inevitably keeping such functions cleanly
distinguished is not always easy).
On the other hand, \R will enforce that only one definition of a
generic function can be associated with a particular combination of
function and package name, in the current session or other active
version of \R.

Tables of methods for a particular generic function, in this sense,
will often be spread over several other packages.
The total set of methods for a given generic function may change
during a session, as additional packages are loaded.
Each table must be consistent in the signature assumed for the generic
function.

\R distinguishes \emph{standard} and \emph{nonstandard} generic
functions, with the former having a function body that does nothing
but dispatch a method.
For the most part, the distinction is just one of simplicity:  knowing
that a generic function only dispatches a method call allows some
efficiencies and also removes some uncertainties.

In most cases, the generic function is the visible function
corresponding to that name, in the corresponding package.
There are two exceptions, \emph{implicit} generic
functions and the special computations required to deal with \R's
\emph{primitive} functions.
Packages can contain a table of implicit generic versions of functions
in the package, if the package wishes to leave a function non-generic
but to constrain what the function would be like if it were generic.
Such implicit generic functions are created during the installation of
the package, essentially by defining the generic function and
possibly methods for it, and then reverting the function to its
non-generic form. (See \link{implicitGeneric} for how this is done.)
The mechanism is mainly used for functions in the older packages in
\R, which may prefer to ignore S4 methods.
Even in this case, the actual mechanism is only needed if something
special has to be specified.
All functions have a corresponding implicit generic version defined
automatically (an implicit, implicit generic function one might say).
This function is a standard generic with the same arguments as the
non-generic function, with the non-generic version as the default (and only)
method, and with the generic signature being all the formal arguments
except \dots.

The implicit generic mechanism is needed only to override some aspect
of the default definition.
One reason to do so would be to remove some arguments from the
signature.
Arguments that may need to be interpreted literally, or for which the
lazy evaluation mechanism of the language is needed, must \emph{not}
be included in the signature of the generic function, since all
arguments in the signature will be evaluated in order to select a
method.
For example, the argument \code{expr} to the function
\code{\link{with}} is treated literally and must therefore be excluded
from the signature.

One would also need to define an implicit generic if the existing
non-generic function were not suitable as the default method.
Perhaps the function only applies to some classes of objects, and the
package designer prefers to have no general default method.
In the other direction, the package designer might have some ideas
about suitable methods for some classes, if the function were generic.
With reasonably modern packages, the simple approach in all these
cases is just to define the function as a generic.
The implicit generic mechanism is mainly attractive for older packages
that do not want to require the methods package to be available.

Generic functions will also be defined but not obviously visible for
functions implemented as \emph{primitive} functions in the base
package.
Primitive functions look like ordinary functions when printed but are
in fact not function objects but objects of two types interpreted by
the \R evaluator to call underlying C code directly.
Since their entire justification is efficiency, \R refuses to hide
primitives behind a generic function object.
Methods may be defined for most primitives, and corresponding metadata
objects will be created to store them.
Calls to the primitive still go directly to the C code, which will
sometimes check for applicable methods.
The definition of \dQuote{sometimes} is that methods must have been
detected for the function in some package loaded in the session and
\code{isS4(x)} is \code{TRUE} for  the first argument (or for the
second argument, in the case of binary operators).
You can test whether methods have been detected by calling
\code{\link{isGeneric}} for the relevant function and you can examine
the generic function by calling \code{\link{getGeneric}}, whether or
not methods have been detected.
For more on generic functions, see the references and also section 2
of the \emph{R Internals} document supplied with \R.

}

\section{Method Definitions}{
All method definitions are stored as objects from the
\code{\linkS4class{MethodDefinition}} class.
Like the class of generic functions, this class extends ordinary \R
functions with some additional slots: \code{"generic"}, containing the
name and package of the generic function, and two signature slots,
\code{"defined"} and \code{"target"}, the first being the signature supplied when
the method was defined by a call to \code{\link{setMethod}}.
The  \code{"target"} slot starts off equal to the \code{"defined"}
  slot.  When an inherited method is cached after being selected, as
  described above, a copy is made with the  appropriate \code{"target"}  signature.
  Output from \code{\link{showMethods}}, for example, includes both
  signatures.

  Method definitions are required to have the same formal arguments as
  the generic function, since the method dispatch mechanism does not
  rematch arguments, for reasons of both efficiency and consistency.
}


\references{
 Chambers, John M. (2016)
 \emph{Extending R},
  Chapman & Hall.
(Chapters 9 and 10.)

 Chambers, John M. (2008)
 \emph{Software for Data Analysis: Programming with R}
  Springer. (Section 10.5 for some details.)
}

\seealso{
For more specific information, see
  \code{\link{setGeneric}}, \code{\link{setMethod}}, and
  \code{\link{setClass}}.

For the use of \dots in methods, see  \link{dotsMethods}.
}
\keyword{programming}
\keyword{classes}
\keyword{methods}
