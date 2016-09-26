% File src/library/methods/man/setOldClass.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2010, 2016 R Core Team
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
  class. Simple usage will be of the form:

\code{setOldClass(Classes)}


where \code{Classes} is the character vector that would be the
  \code{class} attribute of the S3 object. Calls to
  \code{setOldClass()} in the code for a package
  allow the class to be used as a slot in formal classes and in
  signatures for methods (see \link{Methods_for_S3}).

  If the S3 class has a known set of attributes, an
  equivalent S4 class can be specified by \code{S4Class=} in the call to
  \code{setOldClass()}; see the section on \sQuote{Known Attributes}.
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
  \item{S4Class}{ optionally, the class definition or the class name
    of an S4 class.  The new class will have all the slots and other
    properties of this class, plus any S3 inheritance implied by
    multiple names in the \code{Classes} argument.  See the section
    on \dQuote{S3 classes with known attributes} below.
  }
  \item{prototype, where, test}{\emph{These arguments are currently
        allowed, but not recommended in typical applications.}

  \code{prototype}:
    An optional object to use as the prototype.  If the S3 class is
    not to be \code{VIRTUAL} (the default), the use of \code{S4Class=} is
    preferred. 
  
  \code{where}:
    Where to store the class definitions. Should be the default (the
    package namespace) for normal use in an application package.
  
  \code{test}: flag, if \code{TRUE}, arrange to test inheritance
    explicitly for each object, needed if the S3 class can have a
    different set of class strings, with the same first string.
    Such classes are inherently malformed, are rare, and should be avoided.
  }
}
\details{
  The name (or each of the names) in \code{Classes} will be defined as an S4 class, extending class \code{oldClass},
  which is the \sQuote{root} of all old-style classes.  S3 classes
  with multiple names in their class attribute will have a
  corresponding inheritance as formal classes.  See the \code{"mlm"} example.

  S3 classes have
  no formal definition, and therefore no formally defined slots.
  If no S4 class is supplied as a model, the class created will be a
  virtual class.
  The main disadvantage is that the prototype object in an S4 class that
  uses this class as a slot will have a \code{NULL} object in that slot,
  which can sometimes lead to confusion.  You can supply the
  \code{prototype} argument but such examples often can use the
  \code{S4Class=} argument.

  See \link{Methods_Details} for the details of method dispatch and
  inheritance with mixed S3 and S4 methods.

  Some S3 classes cannot be represented as an ordinary combination of S4
  classes and superclasses, because objects with the same initial
  string in the class attribute can have different strings following.
  Such  classes are fortunately rare.  They violate the basic idea of
  object-oriented programming and should be avoided.  
  If you must deal with them, it is still possible to register
  such classes as S4 classes, but now the inheritance has to be verified
  for each object, and you must call \code{setOldClass} with argument
  \code{test=TRUE} once for each superclass.
}

\section{Pre-Defined Old Classes}{

  Many of the widely used S3 classes in the standard R distribution
  come pre-defined for use with S4.  These don't need to be explicitly
  declared in your package (although it does no harm to do so).
 
  The list \code{.OldClassesList} contains the old-style classes that
  are defined by the methods package.  Each element of the list is a
  character vector, with multiple strings if inheritance is included.
  Each element of the list was passed to \code{setOldClass} when
  creating the \pkg{methods} package; therefore, these classes can be used
  in \code{\link{setMethod}} calls, with the inheritance as implied by
  the list.
}

\section{S3 Classes with known attributes}{
  A further specification of an S3 class can be made \emph{if} the
  class is guaranteed to have some attributes of known class (where as
  with slots, \dQuote{known} means that the attribute is an object of
  a specified class, or a subclass of that class).

  In this case, the call to \code{setOldClass()} can supply an S4 class
  definition representing the known structure.  Since S4 slots are
  implemented as attributes (largely for just this reason), the known
  attributes can be specified in the representation of the S4 class.
  The usual technique will be to create an S4 class with the desired
  structure, and then supply the class name or definition as the
  argument \code{S4Class=} to \code{setOldClass()}.

  See the definition of class \code{"ts"} in the examples below and
  the \code{data.frame} example in Section 10.2 of the reference.
  The call to \code{\link{setClass}} to create the S4 class can use the same
  class name, as here, so long as the call to \code{setOldClass}
  follows in the same package.  For clarity it should be the next
  expression in the same file.

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

  Failures of the S3 class to live up to its asserted
  behavior will usually go uncorrected, since S3 classes inherently
  have no definition, and the resulting invalid S4 objects can cause
  all sorts of grief.  Many S3 classes are not candidates for known
  slots, either because the presence or class of the attributes are
  not guaranteed  (e.g., \code{dimnames} in arrays, although these are
  not even S3 classes), or because the class uses named components of
  a list rather than attributes (e.g., \code{"lm"}).  An attribute
  that is sometimes missing cannot be represented as a slot, not even
  by pretending that it is present with class \code{"NULL"}, because
  attributes, unlike slots, can not have value \code{NULL}.

  One irregularity that is usually tolerated, however, is to optionally
  add other attributes to those guaranteed to exist (for example,
  \code{"terms"} in \code{"data.frame"} objects returned by
  \code{\link{model.frame}}).  Validity checks by
  \code{\link{validObject}} ignore extra attributes; even if this check
  is tightened in the future, classes extending S3 classes would likely
  be exempted because extra attributes are so common.
}

\seealso{
  \code{\link{setClass}}, \code{\link{setMethod}}
}


\references{
 Chambers, John M. (2016)
 \emph{Extending R},
  Chapman & Hall.
(Chapters 9 and 10, particularly Section 10.8)
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
setClass("xlm", slots = c(eps = "numeric"), contains = "lm")
setClass("ylm", slots = c(header = "character"), contains = "xlm")
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
setClass("stamped4", contains = "vector", slots = c(date = "POSIXt"))

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
setClass("ts", contains = "structure", slots = c(tsp = "numeric"),
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
