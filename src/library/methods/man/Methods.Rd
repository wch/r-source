% File src/library/methods/man/Methods.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2016 R Core Team
% Distributed under GPL 2 or later

\name{Methods}
\alias{Methods}
\title{General Information on Methods}
\description{
  This documentation section covers some general topics on how methods
  work and how the \pkg{methods} package interacts with the rest of \R.  The
  information is usually not needed to get started with methods and
  classes, but may be helpful for moderately ambitious projects, or when
  something doesn't work as expected.

  The section \dQuote{How Methods Work} describes the underlying
  mechanism; \dQuote{S3 Methods and Generic Functions} gives the rules applied when S4
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

  Each \R package will include  methods metadata objects
  corresponding to each generic function  for which methods have been
  defined in that package.
  When the package is loaded into an \R session, the methods for each
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

\section{Methods for S3 Generic Functions}{

S4 methods may be wanted for functions that also have S3 methods, corresponding to classes for the first
formal argument of an S3 generic function--either a regular R function in which there is a
call to the S3 dispatch function, \code{\link{UseMethod}},
or one of a fixed set of primitive
functions, which are not true functions but go directly to C code.
In either case S3 method dispatch looks at the class of the first
argument or the class of either
argument in a call to one of the primitive binary operators.
S3 methods are ordinary functions with the same arguments as the
generic function (for primitives the formal arguments are not actually
part of the object, but are simulated when the object is printed or
viewed by \code{\link{args}()}).
The \dQuote{signature} of an S3 method is identified  by the name to
which the method is assigned, composed of the name of the
generic function, followed by \code{"."}, followed by the name of the class.
For details, see \link{S3Methods}.

To implement a method for one of these functions corresponding to S4
classes, there are two possibilities: either an S4 method or an S3 method with the
S4 class name.
The S3 method is only possible if the intended signature has the
first argument and nothing else.
In this case,
the recommended approach is to define the S3 method and also supply the
identical function as the definition of the S4 method.
If the S3 generic function was \code{f3(x, ...)} and the S4 class for
the new method was
\code{"myClass"}:

 \code{f3.myClass <- function(x, ...) { ..... }}

 \code{setMethod("f3", "myClass", f3.myClass)}

The reasons for defining both S3 and S4 methods are as follows:
\enumerate{
  \item An S4 method alone will not be seen if the S3 generic function
    is called directly.  However, primitive functions and operators
    are exceptions:  The internal C code will look for S4 methods
    if and only if the object is an S4 object.  In the examples, the method
    for \code{`[`} for class \code{"myFrame"} will always be called
    for objects of this class.

    For the same reason, an S4 method defined for an S3 class will not be called from
    internal code for a non-S4 object. (See the example for function
    \code{Math} and
    class \code{"data.frame"} in the examples.)
  \item An S3 method alone will not be called if there is \emph{any}
    eligible non-default S4 method. (See the example for function
    \code{f3} and class \code{"classA"} in the examples.)
}
Details of the selection computations are given below.

When an S4 method is defined for an existing function that is not an
S4 generic function (whether or not the existing function is an S3 generic),
an S4 generic function will be created corresponding to the existing
function and the package in which it is found (more precisely,
according to the implicit generic function either specified or
inferred from the ordinary function; see \code{\link{implicitGeneric}}).
A message is printed after the initial call to
\code{\link{setMethod}}; this is not an error, just a reminder that
you have created the generic.
Creating the generic explicitly by the call

  \code{setGeneric("f3")}

avoids the message, but has the same effect.
The existing function becomes the default method for
the S4 generic function.
Primitive functions work the same way, but
the S4 generic function is not explicitly created (as discussed below).


S4 and S3 method selection are designed to follow compatible rules of
inheritance, as far as possible.
S3 classes can be used for any S4 method selection, provided that the
S3 classes have been registered by a call to
\code{\link{setOldClass}}, with that call specifying the correct S3
inheritance pattern.
S4 classes can be used for any S3 method selection; when an S4 object
is detected, S3 method selection uses the contents of
\code{\link{extends}(class(x))} as the equivalent of the S3
inheritance (the inheritance is cached after the first call).

An existing S3 method may not behave as desired for an S4 subclass, in
which case utilities such as \code{\link{asS3}} and
\code{\link{S3Part}} may be useful.  If the S3 method fails on the S4
object, \code{asS3(x)} may be passed instead; if the object returned
by the S3 method needs to be incorporated in the S4 object, the
replacement function for \code{S3Part} may be useful, as in the method
for class \code{"myFrame"} in the examples.

Here are details explaining the reasons for defining both S3 and S4 methods.
Calls still accessing the S3 generic function
directly will not see S4 methods, except in the case of primitive
functions.
This means that calls to the generic function from namespaces that
import the S3 generic but not the S4 version will only see S3
methods.
On the other hand, S3 methods will only be selected from the
S4 generic function as part of its default (\code{"ANY"}) method.
If there are inherited S4 non-default methods, these will be chosen in
preference to \emph{any} S3 method.

S3 generic functions implemented as primitive functions (including
binary operators) are an exception to recognizing only
S3 methods.
These functions dispatch both S4 and S3 methods from
the internal C code.
There is no explicit generic function, either S3 or S4.
The internal code looks for S4 methods if the first
argument, or either of the arguments in the case of a binary operator,
is an S4 object.
If no S4 method is found, a search is made for an S3 method.

S4 methods can be defined for an S3 generic function and an S3 class,
but if the function is a primitive, such methods will not be selected
if the object in question is not an S4 object.
In the examples below, for instance,  an S4 method for signature
\code{"data.frame"} for function \code{f3()} would be called for the
S3 object \code{df1}.
A similar S4 method for primitive function
\code{`[`} would be ignored for that object, but would be called for
the S4 object \code{mydf1} that inherits from \code{"data.frame"}.
Defining both an S3 and S4 method removes this inconsistency.


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
Two additional mechanisms for defining
superclasses exist.
A call to \code{\link{setClassUnion}} creates a union class that
is a
superclass of each of the members of the union.
 A call to
\code{\link{setIs}} can create an inheritance relationship that is not the simple one of
containing the superclass representation in the new class.
Arguments \code{coerce} and \code{replace} supply methods to convert
to the superclass and to replace the part corresponding to the superclass.
(In addition, a \code{test=} argument allows conditional inheritance;  conditional inheritance is not
recommended and is not used in method selection.)
All three mechanisms are treated equivalently for purposes of
method selection:  they define the \emph{direct} superclasses of a
particular class.
For more details on the mechanisms, see \code{\link{Classes}}.

The direct superclasses themselves may
have superclasses, defined by any of the same mechanisms, and
similarly through further generations.  Putting all this information together produces
the full list of superclasses for this class.
The superclass list is included in the definition of the class that is
cached during the \R session.
Each element of the list describes the nature of the relationship (see
\code{\linkS4class{SClassExtension}} for details).
Included in the element is a \code{distance} slot containing
the path length for the relationship:
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
lexicographically in the ordering) and what other methods could have
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
## A class that extends a registered S3 class inherits that class' S3
## methods.

setClass("myFrame", contains = "data.frame",
    slots = c(timestamps = "POSIXt"))

df1 <- data.frame(x = 1:10, y = rnorm(10), z = sample(letters,10))

mydf1 <- new("myFrame", df1, timestamps = Sys.time())

## "myFrame" objects inherit "data.frame" S3 methods; e.g., for `[`

mydf1[1:2, ] # a data frame object (with extra attributes)

## a method explicitly for "myFrame" class


setMethod("[",
    signature(x = "myFrame"),
    function (x, i, j, ..., drop = TRUE)
    {
        S3Part(x) <- callNextMethod()
        x@timestamps <- c(Sys.time(), as.POSIXct(x@timestamps))
        x
    }
)

\donttest{mydf1[1:2, ]}


setClass("myDateTime", contains = "POSIXt")

now <- Sys.time() # class(now) is c("POSIXct", "POSIXt")
nowLt <- as.POSIXlt(now)# class(nowLt) is c("POSIXlt", "POSIXt")

mCt <- new("myDateTime", now)
mLt <- new("myDateTime", nowLt)

## S3 methods for an S4 object will be selected using S4 inheritance
## Objects mCt and mLt have different S3Class() values, but this is
## not used.
f3 <- function(x)UseMethod("f3") # an S3 generic to illustrate inheritance

f3.POSIXct <- function(x) "The POSIXct result"
f3.POSIXlt <- function(x) "The POSIXlt result"
f3.POSIXt <- function(x) "The POSIXt result"

stopifnot(identical(f3(mCt), f3.POSIXt(mCt)))
stopifnot(identical(f3(mLt), f3.POSIXt(mLt)))



## An S4 object selects S3 methods according to its S4 "inheritance"


setClass("classA", contains = "numeric",
         slots = c(realData = "numeric"))

Math.classA <- function(x) {(getFunction(.Generic))(x@realData)}
setMethod("Math", "classA", Math.classA)


x <- new("classA", log(1:10), realData = 1:10)

stopifnot(identical(abs(x), 1:10))

setClass("classB", contains = "classA")

y <- new("classB", x)

stopifnot(identical(abs(y), abs(x))) # (version 2.9.0 or earlier fails here)

## an S3 generic: just for demonstration purposes
f3 <- function(x, ...) UseMethod("f3")

f3.default <- function(x, ...) "Default f3"

## S3 method (only) for classA
f3.classA <- function(x, ...) "Class classA for f3"

## S3 and S4 method for numeric
f3.numeric <- function(x, ...) "Class numeric for f3"
setMethod("f3", "numeric", f3.numeric)

## The S3 method for classA and the closest inherited S3 method for classB
## are not found.

f3(x); f3(y) # both choose "numeric" method

## to obtain the natural inheritance, set identical S3 and S4 methods
setMethod("f3", "classA", f3.classA)

f3(x); f3(y) # now both choose "classA" method

## Need to define an S3 as well as S4 method to use on an S3 object
## or if called from a package without the S4 generic

MathFun <- function(x) { # a smarter "data.frame" method for Math group
  for (i in seq(length = ncol(x))[sapply(x, is.numeric)])
    x[, i] <- (getFunction(.Generic))(x[, i])
  x
}
setMethod("Math", "data.frame", MathFun)

## S4 method works for an S4 class containing data.frame,
## but not for data.frame objects (not S4 objects)

try(logIris <- log(iris)) #gets an error from the old method

## Define an S3 method with the same computation

Math.data.frame <- MathFun

logIris <- log(iris)




\dontshow{
removeClass("classA"); removeClass("classB"); rm(x,y)
removeGeneric("f3")
removeClass("myDateTime")
removeMethod("Math", "data.frame"); rm(Math.data.frame, MathFun, logIris)
}

}


\references{
 Chambers, John M. (2008)
 \emph{Software for Data Analysis: Programming with R}
  Springer.  (For the R version: see section 10.6 for method
  selection and section 10.5 for generic functions).

 Chambers, John M.(2009)
 \emph{Developments in Class Inheritance and Method Selection}
 \url{https://statweb.stanford.edu/~jmc4/classInheritance.pdf}.

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
