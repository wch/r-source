% File src/library/methods/man/setMethod.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2016 R Core Team
% Distributed under GPL 2 or later

\name{setMethod}
\alias{setMethod}
\title{ Create and Save a Method }
\description{
Create a method for a generic function, corresponding to a signature of classes for the arguments. Standard usage will be of the form:

\code{setMethod(f, signature, definition)}

where \code{f} is the name of the function, \code{signature} specifies the argument classes for which the method applies and \code{definition} is the function definition for the method. 
}
\usage{
setMethod(f, signature=character(), definition,
          where = topenv(parent.frame()),
          valueClass = NULL, sealed = FALSE)
}
\arguments{
  \item{f}{ The character-string name of the generic function. The unquoted name usually works as well (evaluating to the generic function), except for a few functions in the base package.}
  \item{signature}{ The classes required for some of the arguments. Most applications just require one or two character strings matching the first argument(s) in the signature. More complicated cases follow R's rule for argument matching. See the details below; however, if the signature is not trivial, you should use \code{\link{method.skeleton}} to generate a valid call to \code{setMethod}.}
  \item{definition}{ A function definition, which will become the method
    called when the arguments in a call to \code{f} match the
    classes in \code{signature}, directly or through inheritance.
    The definition must be a function with the same formal arguments
    as the generic; however, \code{setMethod()} will handle methods
    that add arguments, if \code{\dots} is a formal argument to the generic.
  See the Details section.
  }
  \item{where, valueClass, sealed}{\emph{These arguments are allowed
        but either obsolete or rarely appropriate.}

      \code{where}: where to store the definition; should be the
      default, the namespace for the package.

  \code{valueClass}{ Obsolete. }

  \code{sealed}{ prevents the method being redefined, but should never
    be needed when the method is defined in the source code of a
    package.}
}
}
\value{
  The function exists for its side-effect. The definition will be stored in a special metadata object and incorporated in the generic function when the corresponding package is loaded into an R session. 
  
}
\section{Method Selection: Avoiding Ambiguity}{
When defining methods, it's important to ensure that methods are
selected correctly; in particular, packages should be designed to
avoid ambiguous method selection.

To describe method selection, consider first the case where only one
formal argument is in the active signature; that is, there is only one
argument, \code{x} say, for which methods have been defined.
The generic function has a table of methods, indexed by the class for
the argument in the calls to \code{setMethod}.
If there is a method in the table for the class of \code{x} in the
call, this method is selected.

If not, the next best methods would correspond to the direct
superclasses of \code{class(x)}---those appearing in the
\code{contains=} argument when that class was defined.
If there is no method for any of these, the next best would correspond
to the direct superclasses of the first set of superclasses, and so
on.

The first possible source of ambiguity arises if the class has several
direct superclasses and methods have been defined for more than one of
those;
\R will consider these equally valid and report an ambiguous choice.
If your package has the class definition for \code{class(x)}, then you
need to define a method explicitly for this combination of generic
function and class.

When more than one formal argument appears in the method signature, \R
requires the \dQuote{best} method to be chosen  unambiguously for each
argument.
Ambiguities arise when one method is specific about one argument while
another is specific about a different argument.
A call that satisfies both requirements is then ambiguous:  The two
methods look equally valid, which should be chosen?
In such cases the package needs to add a third method requiring both
arguments to match.

The most common examples arise with binary operators.  Methods may be
defined for individual operators, for special groups of operators such as
\code{\link{Arith}} or for group \code{\link{Ops}}.

}
\section{Exporting Methods}{
If a package defines methods for generic functions, those methods
should be exported if any of the classes involved are exported; in
other words, if someone using the package might expect these methods
to be called.
Methods are exported by including an \code{exportMethods()} directive
in the \code{NAMESPACE} file for the package, with the arguments to
the directive being the names of the generic functions for which
methods have been defined.

Exporting methods is always desirable in the sense of declaring what
you want to happen, in that you do expect users to find such methods.
It can be essential in the case that the method was defined for a
function that is not originally a generic function in its own package
(for example, \code{plot()} in the \code{graphics} package).  In this
case it may be that the version of the function in the \R session is
not generic, and your methods will not be called.

Exporting methods for a function also exports the generic version of
the function.
Keep in mind that this does \emph{not} conflict with the function as
it was originally defined in another package; on the contrary, it's
designed to ensure that the function in the \R session dispatches
methods correctly for your classes and continues to behave as expected
when no specific methods apply.  See \link{Methods_Details} for the actual mechanism.
}
\section{Details}{
The call to \code{setMethod} stores the supplied method definition  in
the metadata table for this generic function in the environment,
typically the global environment or the namespace of a package.
In the case of a package, the table object becomes part of the namespace or environment of the
package.
When the package is loaded into a later session, the
methods will be merged into the table of methods in the corresponding
generic function object.

Generic functions are referenced by the combination of the function name and
the package name;
for example, the function \code{"show"} from the package
\code{"methods"}.
Metadata for methods is identified by the two strings; in particular, the
generic function object itself has slots containing its name and its
package name.
The package name of a generic is set according to the package
from which it originally comes; in particular, and frequently, the
package where a non-generic version of the function originated.
For example, generic functions for all the functions in package \pkg{base} will
have \code{"base"} as the package name, although none of them is an
S4 generic on that package.
These include most of the base functions that are primitives, rather than
true functions; see the section on primitive functions in the
documentation for \code{\link{setGeneric}} for details.

Multiple packages can have methods for the same generic function; that
is, for the same combination of generic function name and package
name.
Even though the methods are stored in separate tables in separate
environments, loading the corresponding packages adds the methods to
the table in the generic function itself, for the duration of the session.

 The class
  names in the signature can be any formal class, including basic
  classes such as \code{"numeric"}, \code{"character"}, and
  \code{"matrix"}.  Two additional special class names can appear:
  \code{"ANY"}, meaning that this argument can have any class at all;
  and \code{"missing"}, meaning that this argument \emph{must not}
  appear in the call in order to match this signature.  Don't confuse
  these two:  if an argument isn't mentioned in a signature, it
  corresponds implicitly to class \code{"ANY"}, not to
  \code{"missing"}.  See the example below.  Old-style (\sQuote{S3})
  classes can also be used, if you need compatibility with these, but
  you should definitely declare these classes by calling
  \code{\link{setOldClass}} if you want S3-style inheritance to work.


  Method definitions can
  have default expressions for arguments, but only if
  the generic function must have \emph{some} default expression for the
  same argument. (This restriction is imposed by the way \R manages
  formal arguments.)
  If so, and if the corresponding argument is
  missing in the call to the generic function, the default expression
  in the method is used.  If the method definition has no default for
  the argument, then the expression supplied in the definition of the
  generic function itself is used, but note that this expression will
  be evaluated using the enclosing environment of the method, not of
  the generic function.
  Method selection does
  not evaluate default expressions.
  All actual (non-missing) arguments in the signature of the
  generic function will be evaluated when a method is selected---when
  the call to \code{standardGeneric(f)} occurs.
  Note that specifying class \code{"missing"} in the signature
  does not require any default expressions.

  It is possible to have some differences between the
  formal arguments to a method supplied to \code{setMethod} and those
  of the generic. Roughly, if the generic has \dots as one of its
  arguments, then the method may have extra formal arguments, which
  will be matched from the arguments matching \dots in the call to
  \code{f}.  (What actually happens is that a local function is
  created inside the method, with the modified formal arguments, and the method
  is re-defined to call that local function.)

  Method dispatch tries to match the class of the actual arguments in a
  call to the available methods collected for \code{f}.  If there is a
  method defined for the exact same classes as in this call, that
  method is used.  Otherwise, all possible signatures are considered
  corresponding to the actual classes or to superclasses of the actual
  classes (including \code{"ANY"}).
  The method having the least distance from the actual classes is
  chosen; if more than one method has minimal distance, one is chosen
  (the lexicographically first in terms of superclasses) but a warning
  is issued.
  All inherited methods chosen are stored in another table, so that
  the inheritance calculations only need to be done once per session
  per sequence of actual classes.
  See
  \link{Methods_Details} and Section 10.7 of the reference for more details.

}

\references{
 Chambers, John M. (2016)
 \emph{Extending R},
  Chapman & Hall.
(Chapters 9 and 10.)
}

\examples{

## examples for a simple class with two numeric slots.
## (Run example(setMethod) to see the class and function definitions)
\dontshow{
  setClass("track", slots = c(x="numeric", y = "numeric"))

  cumdist <- function(x, y) c(0., cumsum(sqrt(diff(x)^2 + diff(y)^2)))
  setClass("trackMultiCurve", slots = c(x="numeric", y="matrix", smooth="matrix"),
          prototype = list(x=numeric(), y=matrix(0,0,0), smooth= matrix(0,0,0)))

require(graphics)
}

## methods for plotting track objects 
##
## First, with only one object as argument, plot the two slots
##  y must be included in the signature, it would default to "ANY"
setMethod("plot", signature(x="track", y="missing"),
  function(x,  y, ...) plot(x@x, x@y, ...)
)

## plot numeric data on either axis against a track object
## (reducing the track object to the cumulative distance along the track)
## Using a short form for the signature, which matches like formal arguments
setMethod("plot", c("track", "numeric"),
 function(x, y, ...) plot(cumdist(x@x, x@y), y,  xlab = "Distance",...)
)

## and similarly for the other axis
setMethod("plot", c("numeric", "track"),
 function(x, y, ...) plot(x, cumdist(y@x, y@y),  ylab = "Distance",...)
)

t1 <- new("track", x=1:20, y=(1:20)^2)
plot(t1)
plot(qnorm(ppoints(20)), t1)

## Now a class that inherits from "track", with a vector for data at
## the points 
  setClass("trackData", contains = c("numeric", "track"))


tc1 <- new("trackData", t1, rnorm(20))


## a method for plotting the object
## This method has an extra argument, allowed because ... is an
## argument to the generic function.
setMethod("plot", c("trackData", "missing"),
function(x, y, maxRadius = max(par("cin")), ...) {
  plot(x@x, x@y, type = "n", ...)
  symbols(x@x, x@y, circles = abs(x), inches = maxRadius)
  }
)
plot(tc1)

## Without other methods for "trackData", methods for "track"
## will be selected by inheritance

plot(qnorm(ppoints(20)), tc1)

## defining methods for primitive function.
## Although "[" and "length" are not ordinary functions
## methods can be defined for them.
setMethod("[", "track",
  function(x, i, j, ..., drop) {
    x@x <- x@x[i]; x@y <- x@y[i]
    x
  })
plot(t1[1:15])

setMethod("length", "track", function(x)length(x@y))
length(t1)

## Methods for binary operators
## A method for the group generic "Ops" will apply to all operators
## unless a method for a more specific operator has been defined.

## For one trackData argument, go on with just the data part
setMethod("Ops", signature(e1 = "trackData"),
    function(e1, e2) callGeneric(e1@.Data, e2))

setMethod("Ops", signature(e2 = "trackData"),
    function(e1, e2) callGeneric(e1, e2@.Data))

## At this point, the choice of a method for a call with BOTH
## arguments from "trackData" is ambiguous.  We must define a method.

setMethod("Ops", signature(e1 = "trackData", e2 = "trackData"),
    function(e1, e2) callGeneric(e1@.Data, e2@.Data))
## (well, really we should only do this if the "track" part
## of the two arguments matched)

tc1 +1

1/tc1

all(tc1 == tc1)

\dontshow{
removeClass("trackData")
removeClass("track")
}
}

\seealso{

\link{Methods_for_Nongenerics} discusses method definition for
functions that are not generic functions in their original package;
\link{Methods_for_S3} discusses the integration of formal methods with the
older S3 methods.

\code{\link{method.skeleton}}, which is the recommended way to generate a skeleton of the call to \code{setMethod}, with the correct formal arguments and other details.

\link{Methods_Details} and the links there for a general discussion, \code{\link{dotsMethods}} for methods that dispatch on
  \dQuote{\dots}, and \code{\link{setGeneric}} for generic functions.
}

\keyword{programming}
\keyword{classes}
\keyword{methods}
