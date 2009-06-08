% File src/library/methods/man/Methods.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Development Team
% Distributed under GPL 2 or later

\name{Methods}
\alias{Methods}
\title{General Information on Methods}
\description{
  This documentation section covers some general topics on how methods
  work and how the \pkg{methods} package interacts with the rest of R.  The
  information is usually not needed to get started with methods and
  classes, but may be helpful for moderately ambitious projects, or when
  something doesn't work as expected.

  The section \dQuote{How Methods Work} describes the underlying
  mechanism; \dQuote{S3 Methods} gives the rules applied when S4
  classes and methods interact with older S3 methods; \dQuote{Method Selection and Dispatch} provides more
  details on how class definitions determine which methods are used;
  \dQuote{Generic Functions} discusses generic functions as objects.
  For additional information specifically about class definitions, see \code{\link{Classes}}.
}

\section{How Methods Work}{
  A generic function  has associated with it a
  collection of other functions (the methods), all of which have the same
  formal arguments as the generic.  See the \dQuote{Generic
    Functions} section below for more on generic functions themselves.

  Each R package will include  methods metadata objects
  corresponding to each generic function  for which methods have been
  defined in that package.
  When the package is loaded into an R session, the methods for each
  generic function are \emph{cached}, that is, stored in the
  environment of the generic function along with the methods from
  previously loaded packages.  This merged table of methods is used to
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
  function is called, rather than using the traditional lazy
  evaluation rules of S.  Therefore, it's important to \emph{exclude}
  from the signature any arguments that need to be dealt with
  symbolically (such as the first argument to function
  \code{\link{substitute}}).  Note that only actual arguments are
  evaluated, not default expressions.
  A missing argument enters into the method selection as class
  \code{"missing"}.

  The cached methods are stored in an
  environment object.  The names used for assignment are a
  concatenation of the class names for the active arguments in the method signature.

}

\section{S3 Methods}{

The functions for which S4 methods will be written often include some
for which S3 methods exist, corresponding to S3 classes for the first
formal argument of an S3 generic function or of a primitive function, or for either of the
arguments in a call to one of the primitive binary operators.
In the case of true functions, S3 methods will be dispatched
by the original version of the function.  The usual
way this happens is by the function becoming the default
method for the S4 generic, implicitly by a call to
\code{\link{setMethod}} or explicitly by the call

  \code{setGeneric("f")}

where the original \code{f()} contained the call
\code{UseMethod("f")}.
The S4 method selection code matches the classes of the arguments as
described in the previous section.
Matching will be applied for the class of S3 objects as well as S4
objects, but only the first string in an S3 class attribute is used.
If no non-default S4 method
matches the call,  the default S4 method can then operate as an S3
generic to select S3 methods for \code{f()}.

Primitive functions and operators dispatch both S4 and S3 methods from
the internal C code.
The method selection mechanism works essentially the same way, with
two exceptions.
There is no explicit generic function, either S3 or S4, meaning that
the selection of an S3 method if no S4 method is found is built in and
not a result of an explicit default method.
Also, the internal code does not look for S4 methods unless the first
argument or one of the arguments to an operator is an S4 object.
S4 methods can be defined for an S3 generic function and an S3 class.
But if the function is a primitive, such methods will not be selected
if the object in question is not an S4 object.
In the examples below, for instance,  an S4 method for signature
\code{"data.frame"} for function \code{f3()} would be called for the
S3 object \code{df1}.
A similar S4 method for primitive function
\code{`[`} would be ignored for that object, but would be called for
the S4 object \code{mydf1} that inherits from \code{"data.frame"}.
It's an unfortunate inconsistency, but enforced by the passion for
efficiency in dispatching methods for primitives.


The common case is that objects from S4 classes will use S4 methods,
except when the function's default definition is wanted.  For example,
if an S4 class extends one of the basic object types the base code for
that type may do what we want.
Objects not from an S4 class will continue to follow S3 method selection.

The rest of this section describes S3 method selection in two special cases.
In one case, the S4 class contains an S3 class (and has ensured that
objects have all the structure needed for the S3 class).
In the second case, S3 methods have been written for an S4 class;
that is, a function \emph{f.class}, where \emph{f} is an S3 generic
function and \emph{class} is the name of an S4 class, other than a
registered S3 class.
The first case is now supported and recommended, the second case is
discouraged, but occasionally needed (see section 4 of the paper in
the references).

The following rules define selection of an S3 method for an S4
object.  S4 objects are defined internally by a bit in the C
structure.  In practice, any object generated from an S4 class will be
an S4 object, as well as the result of most computations
on such objects.  Older computations defined for non-S4 classes or
object types may or may not return S4 objects when the arguments are
such objects.

An S3 method will be selected applying the following criteria in order:
\enumerate{

\item 
the \emph{class} for the method matches the name of the S4 class
exactly;


\item 
the object has a slot \code{".S3Class"} and \emph{class} is selected
by S3 inheritance, treating that slot as the S3 class of the object;

}
The second criterion will apply if either the S4 class contains an S3
class or the argument \code{S3methods=TRUE} was given to
\code{\link{setClass}} for the class of the object or for one of its
superclasses.


If an S4 class extends an S3 class, and if no S4 methods take precedence, we expect that the
correct S3 method for the inherited S3 class will be chosen.
This will happen, so long as the S3 class has been registered by a
call to \code{\link{setOldClass}}.
If so, the object from the S4 class will inherit a special slot
that will be used as the class for S3 dispatch.  Effectively, this
slot is a proxy for the class attribute expected by S3 dispatch.  It
can even vary in its inheritance between objects, as happens with some
S3 classes, such as \code{\link{POSIXt}}, if the replacement version
of \code{\link{S3Class}} is used to set it.
If the class so selected is one of the basic S3 classes,
the object is converted to an S3 object
with this vector as its class attribute.

A second nonstandard situation arises when an S3 method has been
explicitly written for an S4 class.
Versions of R through 2.9.0 did not recognize S4
inheritance in dispatching S3 methods, so that subclasses of the S4 class
would not then inherit the S3 method.  The version of \R{} accompanying this documentation
fixes this problem, to the extent practical, as follows.
S3 method selection will
resemble S4 selection for the same class \emph{if} the call to
\code{\link{setClass}} has included the argument \code{S3methods =
  TRUE}. If not, the current behavior (R 2.9.1) is to select S3 methods
defined for this class, but not for its subclasses  (largely for back compatibility; in future versions of R, S3 methods may be
ignored for S4 classes unless \code{S3methods} is set.)
The implementation uses the same special slot as above for inheriting
from an S3 class.  Subclasses of a class set this way will inherit the
same special slot and the same S3 method selection.
It's even possible to set the slot in individual objects, as above,
but the possibilities for confusion are serious.


Looking in the other direction, it remains true that S4 selection has no
knowledge of S3 methods.
This can cause problems when a class that expects to inherit
the S3 method, \code{"classA"} in the example below, also inherits from another S4
class.  If that class inherits an S4 method for a function, no matter how
indirectly, that S4 method will be selected for an object from
\code{"classA"}, even though there is a directly defined S3 method.
The S3 method can only be accessed through the default S4 method.
These problems are relatively unlikely to occur, but anyone defining a
class that extends both S3 and S4 classes needs to be careful.

}
\section{Method Selection and Dispatch: Details}{

When a call to a generic function is evaluated, a method is selected corresponding
to the classes of the actual arguments in the signature.
First, the cached methods table is searched for an  exact match;
that is, a method stored under the signature defined by
the string value of \code{class(x)} for each non-missing
argument, and \code{"missing"} for each missing argument.
If no method is found directly for the actual arguments in a call to a
generic function, an attempt is made to match the available methods to
the arguments by using the superclass information about the actual classes.

Each class definition may include a list of  one or more
\emph{superclasses} of the new class.
The simplest and most common specification is by the \code{contains=} argument in
the  call to \code{\link{setClass}}.
Each class named in this argument is a superclass of the new class.
The S language has two additional mechanisms for defining
superclasses.
 A call to 
\code{\link{setIs}} can create an inheritance relationship that is not the simple one of
containing the superclass representation in the new class.
In this case, explicit methods are defined to relate the subclass and
the superclass.
Also, a call to \code{\link{setClassUnion}} creates a union class that
is a
superclass of each of the members of the union.
All three mechanisms are treated equivalently for purposes of
method selection:  they define the \emph{direct} superclasses of a
particular class.
For more details on the mechanisms, see \code{\link{Classes}}.

The direct superclasses themselves may
have superclasses, defined by any of the same mechanisms, and
similarly for further generations.  Putting all this information together produces
the full list of superclasses for this class.
The superclass list is included in the definition of the class that is
cached during the R session.
Each element of the list describes the nature of the relationship (see
\code{\linkS4class{SClassExtension}} for details).
Included in the element is a \code{distance} slot giving a numeric
distance between the two classes.
The distance is the path length for the relationship:
\code{1} for direct superclasses (regardless of which mechanism
defined them), then \code{2} for the direct superclasses of those
classes, and so on.
In addition, any class implicitly has class \code{"ANY"} as a superclass.  The
distance to \code{"ANY"} is treated as larger than the distance to any
actual class.
The special class \code{"missing"} corresponding to missing arguments
has only \code{"ANY"} as a superclass, while \code{"ANY"} has no
superclasses.

When a class definition is created or modified, the superclasses
are ordered, first by a stable sort of the all superclasses by
distance.
If the set of superclasses has duplicates (that is, if some class is
inherited through more than one relationship), these are removed, if
possible, so that the list of superclasses is consistent with the
superclasses of all direct superclasses.
See the reference on inheritance for details.

The information about superclasses is summarized when a class
definition is printed.

When a method is to be selected by inheritance, a search is made in
the table for all methods directly corresponding to a combination of
either the direct class or one of its superclasses, for each argument
in the active signature.
For an example, suppose there is only one argument in the signature and that the class of
the corresponding object was \code{"dgeMatrix"} (from the recommended package
\code{Matrix}).
This class has two direct superclasses and through these 4 additional superclasses.
Method selection finds all the methods in the table of directly
specified methods labeled by one of these classes, or by
\code{"ANY"}.

When there are multiple arguments in the signature, each argument will
generate a similar  list of inherited classes.
The possible matches are now all the combinations of classes from each
argument (think of the function \code{outer} generating an array of
all possible combinations).
The search now finds all the methods matching any of this combination
of classes.
For each argument, the position in the list of superclasses of that
argument's class defines which method or methods (if the same class
appears more than once) match best.
When there is only one argument, the best match is unambiguous.
With more than one argument, there may be zero or one match that is
among the best matches for \emph{all} arguments.

If there is no best match, the selection is ambiguous and a message is
printed noting which method was selected (the first method
lexicographicaly in the ordering) and what other methods could have
been selected.
Since the ambiguity is usually nothing the end user could control,
this is not a warning.
Package authors should examine their package for possible ambiguous
inheritance by calling \code{\link{testInheritedMethods}}.

When the inherited method has been selected, the selection is cached
in the generic function so that future calls with the same class will
not require repeating the search.  Cached inherited selections are
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
first reference.

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
For more on generic functions, see the first reference and also section 2 of \emph{R Internals}.

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

\examples{
## The rules for inheriting S3 methods.

f3 <- function(x)UseMethod("f3") # an S3 generic to illustrate inheritance

## A class that extends a registered S3 class inherits that class' S3
## methods.  The S3 methods will be passed an object with the S3 class

setClass("myFrame", contains = "data.frame",
    representation(date = "POSIXt", type = "character"))

df1 <- data.frame(x = 1:10, y = rnorm(10), z = sample(letters,10))

mydf1 <- new("myFrame", df1, date = Sys.time())

## "myFrame" objects inherit "data.frame" S3 methods; e.g., for `[`

mydf1[1:2, ] # a data frame object (with extra attributes "date" and "type")

\dontshow{

m1 <- mydf1[1:2,]
attr(m1, "date") <- attr(m1, "type") <- NULL
stopifnot(identical(m1, df1[1:2,]))
}

## Extending an S3 class with inconsistent (instance-based) inheritance
setClass("myDateTime", contains = "POSIXt")

now <- Sys.time() # class(now) is c("POSIXt", "POSIXct")
nowLt <- as.POSIXlt(now)# class(nowLt) is c("POSIXt", "POSIXlt")

mCt <- new("myDateTime", now)
mLt <- new("myDateTime", nowLt)

## S3 methods will be selected using instance-based information

f3.POSIXct <- function(x) "The POSIXct result"
f3.POSIXlt <- function(x) "The POSIXlt result"

stopifnot(identical(f3(mCt), f3.POSIXct(mCt)))
stopifnot(identical(f3(mLt), f3.POSIXlt(mLt)))



## An S4 class that does not contain a registered S3 class or object type
## selects S3 methods according to its S4 "inheritance"
## but only if the class definition requests this via S3methods=TRUE
## ( from version 2.9.1 on)

setClass("classA", contains = "numeric",
   representation(realData = "numeric"), S3methods = TRUE)

Math.classA <- function(x) {(getFunction(.Generic))(x@realData)}

x <- new("classA", log(1:10), realData = 1:10)

stopifnot(identical(abs(x), 1:10))

setClass("classB", contains = "classA")

y <- new("classB", x)

stopifnot(identical(abs(y), 1:10)) # (version 2.9.0 or earlier fails here)

## Note: with a class that tries to combine both S3 and S4 superclasses.
## The S3 inheritance is used and the S3 method for
## the S4 superclass will not be selected.

setClass("classC", representation(x = "numeric"))

# an S3 method for "[" (not a good idea, but it would work)
`[.classc` <- function(x, ..., drop = TRUE) {x@x[...]}


setClass("classD", contains = c("classC", "data.frame"))

## by the rule mentioned in the S3 method section, the
## S3 methods are selected from the S3 class defined; that is, "data.frame"
## If the user expected to inherit `[.classC`, no luck.
xd <- new("classD", df1, x = 1:50)

## Note the error from `[.data.frame`
try(xd[1:25])

\dontshow{
removeClass("classA"); removeClass("classB"); rm(x,y)
removeClass("myDateTime")
}

}


\references{
 Chambers, John M. (2008)
 \emph{Software for Data Analysis: Programming with R}
  Springer.  (For the R version: see section 10.6 for method
  selection and section 10.5 for generic functions).

 Chambers, John M.(2009)
 \emph{Developments in Class Inheritance and Method Selection}
 \url{http://stat.stanford.edu/~jmc4/classInheritance.pdf}.

 Chambers, John M. (1998)
 \emph{Programming with Data}
 Springer (For the original S4 version.)
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
