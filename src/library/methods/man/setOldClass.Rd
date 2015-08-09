% File src/library/methods/man/setOldClass.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2010 R Core Team
% Distributed under GPL 2 or later

\name{setOldClass}
\alias{setOldClass}
\alias{.setOldIs}
\alias{POSIXct-class}
\alias{POSIXlt-class}
\alias{POSIXt-class}
\alias{aov-class}
\alias{maov-class}
\alias{anova-class}
\alias{anova.glm-class}
\alias{anova.glm.null-class}
\alias{Date-class}
\alias{data.frame-class}
\alias{data.frameRowLabels-class}
\alias{density-class}
\alias{dump.frames-class}
\alias{factor-class}
\alias{formula-class}
\alias{glm-class}
\alias{glm.null-class}
\alias{hsearch-class}
\alias{integrate-class}
\alias{libraryIQR-class}
\alias{lm-class}
\alias{logLik-class}
\alias{mlm-class}
\alias{mtable-class}
\alias{mts-class}
\alias{ordered-class}
\alias{packageIQR-class}
\alias{packageInfo-class}
\alias{recordedplot-class}
\alias{rle-class}
\alias{socket-class}
\alias{summaryDefault-class}
\alias{summary.table-class}
\alias{oldClass-class}
\alias{.OldClassesList}
\alias{table-class}
\alias{initialize,data.frame-method}
\alias{initialize,factor-method}
\alias{initialize,ordered-method}
\alias{initialize,table-method}
\alias{initialize,summary.table-method}
\title{Register Old-Style (S3) Classes and Inheritance}
\description{
  Register an old-style (a.k.a. \sQuote{S3}) class as a formally defined
  class. The \code{Classes} argument is the character vector used as the
  \code{class} attribute; in particular, if there is more than one
  string, old-style class inheritance is mimicked.  Registering via
  \code{setOldClass} allows S3 classes to appear in method signatures,
  as a slot in an S4 class, or as a superclass of an S4 class.
}
\usage{
setOldClass(Classes, prototype, where, test = FALSE, S4Class)
}
\arguments{
  \item{Classes}{
    A character vector, giving the names for S3
    classes, as they would appear on the right side of an assignment of
    the \code{class} attribute in S3 computations.

    In addition to S3 classes, an object type or other valid data part
    can be specified, if the S3 class is known to require its data to
    be of that form.
  }
\item{prototype}{
    An optional object to use as the prototype.  This should be provided
    as the default S3 object for the class.  If omitted, the S4 class
    created to register the S3 class is \code{VIRTUAL}.  See the
    details.
  }
  \item{where}{
    Where to store the class definitions, the global or top-level
    environment by default.  (When either function is called in the
    source for a package, the class definitions will be included in the
    package's environment by default.)
  }
  \item{test}{flag, if \code{TRUE}, arrange to test inheritance
    explicitly for each object, needed if the S3 class can have a
    different set of class strings, with the same first string.
    This is a different mechanism in implementation and should be
    specified separately for each pair of classes that have an
    optional inheritance.  See the \sQuote{Details}.
  }
  \item{S4Class}{ optionally, the class definition or the class name
    of an S4 class.  The new class will have all the slots and other
    properties of this class, plus its S3 inheritance as defined by
    the \code{Classses} argument.  Arguments \code{prototype} and
    \code{test} must not be supplied in this case.  See the section
    on \dQuote{S3 classes with known attributes} below.
  }
}
\details{
  Each of the names will be defined as an S4 class, extending the
  remaining classes in \code{Classes}, and the class \code{oldClass},
  which is the \sQuote{root} of all old-style classes. S3 classes have
  no formal definition, and therefore no formally defined slots. If a
  \code{prototype} argument is supplied in the call to
  \code{setOldClass()}, objects from the class can be generated, by a
  call to \code{\link{new}}; however, this usually not as relevant as
  generating objects from subclasses (see the section on extending S3
  classes below).   If  a prototype is not provided,
   the class will be created as a virtual S4 class.
  The main disadvantage is that the prototype object in an S4 class that
  uses this class as a slot will have a \code{NULL} object in that slot,
  which can sometimes lead to confusion.

  Beginning with version 2.8.0 of \R, support is provided for using a
  (registered) S3 class as a superclass of a new S4 class.  See the
  section on extending S3 classes below, and the examples.

  See \link{Methods} for the details of method dispatch and
  inheritance.

  Some S3 classes cannot be represented as an ordinary combination of S4
  classes and superclasses, because objects from the S3 class can have a
  variable set of strings in the class. It is still possible to register
  such classes as S4 classes, but now the inheritance has to be verified
  for each object, and you must call \code{setOldClass} with argument
  \code{test=TRUE} once for each superclass.

  For example, ordered factors \emph{always} have the S3
  class \code{c("ordered", "factor")}.  This is proper behavior, and
  maps simply into two S4 classes, with \code{"ordered"} extending
  \code{"factor"}.

  But objects whose class attribute has \code{"POSIXt"} as the first
  string may have either (or neither) of \code{"POSIXct"} or
  \code{"POSIXlt"} as the second string.  This behavior can be mapped
  into S4 classes but now to evaluate \code{is(x, "POSIXlt")}, for
  example, requires checking the S3 class attribute on each object.
  Supplying the \code{test=TRUE} argument to \code{setOldClass} causes
  an explicit test to be included in the class definitions.  It's
  never wrong to have this test, but since it adds significant
  overhead to methods defined for the inherited classes, you should
  only supply this argument if it's known that object-specific tests
  are needed.

  The list \code{.OldClassesList} contains the old-style classes that
  are defined by the methods package.  Each element of the list is a
  character vector, with multiple strings if inheritance is included.
  Each element of the list was passed to \code{setOldClass} when
  creating the \pkg{methods} package; therefore, these classes can be used
  in \code{\link{setMethod}} calls, with the inheritance as implied by
  the list.
}

\section{Extending S3 classes}{
  A call to \code{setOldClass} creates formal classes corresponding
  to S3 classes, allows these to be used as slots in other classes or in
  a signature in \code{\link{setMethod}}, and mimics the S3
  inheritance.

  In documentation for the initial implementation of S4 classes in \R,
  users were warned against defining S4 classes that contained S3
  classes, even if those had been registered.  The warning was based
  mainly on two points.  1: The S3 behavior of the objects would fail
  because the S3 class would not be visible, for example, when S3
  methods are dispatched.  2: Because S3 classes have no formal
  definition, nothing can be asserted in general about the S3 part of
  an object from such a class.  (The warning was repeated as recently
  as the first reference below.)

  Nevertheless, defining S4 classes to contain an S3 class and extend
  its behavior is attractive in many applications.  The alternative is
  to be stuck with S3 programming, without the flexibility and security of
  formal class and method definitions.

  Beginning with version 2.8.0, \R provides support for extending
  registered S3 classes; that is, for new classes defined by a call to
  \code{\link{setClass}} in which the \code{contains=} argument
  includes an S3 class.  See the examples below.  The support is aimed
  primarily at providing the S3 class information for all classes that
  extend class \linkS4class{oldClass}, in particular by ensuring that
  all objects from such classes contain the S3 class in a special
  slot.

  There are three different ways to indicate an extension to an existing
  S3 class:  \code{setOldClass()},  \code{setClass()} and
  \code{setIs()}.  In most cases, calling \code{setOldClass} is the
  best approach,  but the alternatives may be preferred in the special
  circumstances described below.

  Suppose \code{"A"} is any class extending \code{"oldClass"}. then

  \code{setOldClass(c("B", "A"))}

  creates a new class \code{"B"} whose S3 class
  concatenates \code{"B"}  with \code{S3Class("A")}.  The new class is a
  virtual class.  If \code{"A"} was defined with known attribute/slots,
  then \code{"B"} has these slots also; therefore, you must believe that
  the corresponding S3 objects from class \code{"B"} do indeed have the
  claimed attributes.  Notice that you can supply an S4 definition for the
  new class to specify additional attributes (as described in the next
  section.)
  The first alternative call produces a non-virtual class.

  \code{setClass("B", contains = "A")}

  This creates a non-virtual class with the same slots and superclasses
  as class \code{"A"}.  However, class \code{"B"} is not included in
  the S3 class slot of the new class, unless you provide it explicitly
  in the prototype.

  \code{setClass("B"); setIs("B", "A", .....)}

  This creates a virtual class that extends \code{"A"}, but does not
  contain the slots of \code{"A"}.  The additional arguments to
  \code{\link{setIs}} should provide a coerce and replacement method.
  In order for the new class to inherit S3 methods, the coerce method must
  ensure that the class \code{"A"} object produced has a suitable S3
  class.  The only likely reason to prefer this third approach is that
  class \code{"B"} is not consistent with known attributes in class
  \code{"A"}.

  Beginning with version 2.9.0 of \R, objects from a class extending an
  S3 class will be converted to the corresponding S3 class when being
  passed to an S3 method defined for that class (that is, for one of the
  strings in the S3 class attribute).  This is intended to ensure, as
  far as possible, that such methods will work if they work for ordinary
  S3 objects.  See \code{\link{Classes}} for details.
}

\section{S3 Classes with known attributes}{
  A further specification of an S3 class can be made \emph{if} the
  class is guaranteed to have some attributes of known class (where as
  with slots, \dQuote{known} means that the attribute is an object of
  a specified class, or a subclass of that class).

  In this case, the call to \code{setOldClass()} can supply an S4 class
  definition representing the known structure.  Since S4 slots are
  implemented as attributes (largely for just this reason), the know
  attributes can be specified in the representation of the S4 class.
  The usual technique will be to create an S4 class with the desired
  structure, and then supply the class name or definition as the
  argument \code{S4Class} to \code{setOldClass()}.

  See the definition of class \code{"ts"} in the examples below.  The
  call to \code{\link{setClass}} to create the S4 class can use the same
  class name, as here, so long as the class definition is not sealed.
  In the example, we define \code{"ts"} as a vector structure with a
  numeric slot for \code{"tsp"}.  The validity of this definition relies
  on an assertion that all the S3 code for this class is consistent with
  that definition; specifically, that all \code{"ts"} objects will
  behave as vector structures and will have a numeric \code{"tsp"}
  attribute. We believe this to be true of all the base code in \R, but
  as always with S3 classes, no guarantee is possible.

  The S4 class definition can  have virtual superclasses (as in
  the \code{"ts"} case) if the S3 class is asserted to behave
  consistently with these (in the example, time-series objects are
  asserted to be consistent with the \linkS4class{structure} class).

  For another example, look at the S4 class definition for \code{"data.frame"}.

  Be warned that failures of the S3 class to live up to its asserted
  behavior will usually go uncorrected, since S3 classes inherently
  have no definition, and the resulting invalid S4 objects can cause
  all sorts of grief.  Many S3 classes are not candidates for known
  slots, either because the presence or class of the attributes are
  not guaranteed  (e.g., \code{dimnames} in arrays, although these are
  not even S3 classes), or because the class uses named components of
  a list rather than attributes (e.g., \code{"lm"}).  An attribute
  that is sometimes missing cannot be represented as a slot, not even
  by pretending that it is present with class \code{"NULL"}, because
  attributes unlike slots can not have value \code{NULL}.

  One irregularity that is usually tolerated, however, is to optionally
  add other attributes to those guaranteed to exist (for example,
  \code{"terms"} in \code{"data.frame"} objects returned by
  \code{\link{model.frame}}).  As of version 2.8.0, validity checks by
  \code{\link{validObject}} ignore extra attributes; even if this check
  is tightened in the future, classes extending S3 classes would likely
  be exempted because extra attributes are so common.
}

\seealso{
  \code{\link{setClass}}, \code{\link{setMethod}}
}

\references{
  Chambers, John M. (2008)
  \emph{Software for Data Analysis: Programming with R}
  Springer.  (For the R version: see section 10.6 for method
  selection and section 13.4 for generic functions).

  Chambers, John M. (1998)
  \emph{Programming with Data}
  Springer (For the original S4 version.)
}

\examples{
require(stats)
setOldClass(c("mlm", "lm"))
setGeneric("dfResidual", function(model)standardGeneric("dfResidual"))
setMethod("dfResidual", "lm", function(model)model$df.residual)

## dfResidual will work on mlm objects as well as lm objects
myData <- data.frame(time = 1:10, y = (1:10)^.5)
myLm <- lm(cbind(y, y^3)  ~ time, myData)

showClass("data.frame")# to see the predefined S4 "oldClass"

## two examples extending S3 class "lm", class "xlm" directly
## and "ylm" indirectly
setClass("xlm", representation(eps = "numeric"), contains = "lm")
setClass("ylm", representation(header = "character"), contains = "xlm")
ym1 = new("ylm", myLm, header = "Example", eps = 0.)
## for more examples, see ?\link{S3Class}.

utils::str(.OldClassesList)

\dontshow{
stopifnot(identical(dfResidual(myLm), myLm$df.residual))
removeClass("ylm"); removeClass("xlm")
rm(myData, myLm)
removeGeneric("dfResidual")}

## Examples of S3 classes with guaranteed attributes
## an S3 class "stamped" with a vector and  a "date" attribute
## Here is a generator function and an S3 print method.
## NOTE:  it's essential that the generator checks the attribute classes
stamped <- function(x, date = Sys.time()) {
    if(!inherits(date, "POSIXt"))
      stop("bad date argument")
    if(!is.vector(x))
      stop("x must be a vector")
    attr(x, "date") <- date
    class(x) <- "stamped"
    x
}

print.stamped <- function(x, ...) {
    print(as.vector(x))
    cat("Date: ",  format(attr(x,"date")), "\n")
}

## Now, an S4 class with the same structure:
setClass("stamped4", contains = "vector", representation(date = "POSIXt"))

## We can use the S4 class to register "stamped", with its attributes:
setOldClass("stamped", S4Class = "stamped4")
selectMethod("show", "stamped")
## and then remove "stamped4" to clean up
removeClass("stamped4")
\dontshow{
set.seed(113)}
someLetters <- stamped(sample(letters, 10),
                       ISOdatetime(2008, 10, 15, 12, 0, 0))

st <- new("stamped", someLetters)
st
# show() method prints the object's class, then calls the S3 print method.

stopifnot(identical(S3Part(st, TRUE), someLetters))

# creating the S4 object directly from its data part and slots
new("stamped", 1:10, date = ISOdatetime(1976, 5, 5, 15, 10, 0))

\dontrun{
## The code in R that defines "ts" as an S4 class
setClass("ts", contains = "structure",
      representation(tsp = "numeric"),
      prototype(NA, tsp = rep(1,3)))
# prototype to be a legal S3 time-series
## and now registers it as an S3 class
    setOldClass("ts", S4Class = "ts", where = envir)
}

\dontshow{
  removeClass("stamped")
  rm(someLetters, st)
}

}
\keyword{ programming }
\keyword{ methods }
