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
\alias{data.frame-class}
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
\alias{summary.table-class}
\alias{oldClass-class}
\alias{.OldClassesList}
\alias{table-class}
\title{ Specify Names for Old-Style Classes }
\description{
  Register an old-style (a.k.a. `S3') class as a formally defined
  class. The \code{Classes} argument is the character vector used as the
  \code{class} attribute; in particular, if there is more than one
  string,  old-style class inheritance is mimiced.  Registering via
  \code{setOldClass} allows S3 classes to appear as slots or in method
  signatures.
}
\usage{
setOldClass(Classes, where, test = FALSE)
}
\arguments{
  \item{Classes}{
    A character vector, giving the names for old-style
    classes, as they would appear on the right side of an assignment of
    the \code{class} attribute.
  }
  \item{where}{
    Where to store the class definitions, the global or top-level
    environment by default.  (When either function is called in the
    source for a package, the class definitions will be included in the
    package's environment by default.)
  }
  \item{test}{flag, if \code{TRUE}, inheritance must be tested
    explicitly for each object, needed if the S3 class can have a
    different set of class strings, with the same first string.
    See the details below.
  }
}
\details{
  Each of the names will be defined as a virtual class, extending the
  remaining classes in \code{Classes}, and the class
  \code{oldClass}, which is the \dQuote{root} of all old-style classes.
  See \link{Methods} for the details of method dispatch and
  inheritance. See the section \bold{Register or Convert?} for
  comments on the alternative of defining \dQuote{real} S4 classes
  rather than using \code{setOldClass}.

  S3 classes have no formal definition, and some of them cannot be
  represented as an ordinary combination of S4 classes and
  superclasses.  It is still possible to register the classes as S4
  classes, but now the inheritance has to be verified for each
  object, and you must call \code{setOldClass} with argument
  \code{test=TRUE}.

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
  are defined by the methods package.  Each element of the list is an
  old-style list, with multiple character strings if inheritance is
  included.
  Each element of the list was passed to \code{setOldClass} when
  creating the \pkg{methods} package; therefore, these classes can be used
  in \code{\link{setMethod}} calls, with the inheritance as implied by
  the list.
}

\section{Register or Convert?}{
  A call to \code{setOldClass} creates formal classes corresponding
  to S3 classes, allows these to be used as slots in other classes or in
  a signature in \code{\link{setMethod}}, and mimics the S3 inheritance.

  However, all such classes are created as virtual classes, meaning that
  you cannot generally create new objects from the class by calling
  \code{\link{new}}, and that objects cannot be coerced automatically
  from or to these classes.  All these restrictions just reflect the
  fact that nothing is inherently known about the \dQuote{structure} of
  S3 classes, or whether in fact they define a consistent set of
  attributes that can be mapped into slots in a formal class definition.

  \emph{If} your class does in fact have a consistent structure, so that
  every object from the class has the same structure, you may prefer to
  take some extra time to write down a specific definition in a call to
  \code{\link{setClass}} to convert the class to a fully functional
  formal class.  On the other hand, if the actual contents of the class
  vary from one object to another, you may have to redesign most of the
  software using the class, in which case converting it may not be worth
  the effort.  You should still register the class via
  \code{setOldClass}, unless its class attribute is hopelessly unpredictable.

  An S3 class has consistent structure if each object has the same set
  of attributes, both the names and the classes of the attributes being
  the same for every object in the class.  In practice, you can convert
  classes that are slightly less well behaved.  If a few attributes
  appear in some but not all objects, you can include these optional
  attributes as slots that \emph{always} appear in the objects, if you
  can supply a default value that is equivalent to the attribute being
  missing.  Sometimes \code{NULL} can be that value: A slot (but not an
  attribute) can have the value \code{NULL}.  If \code{version}, for
  example, was an optional attribute, the old test
  \code{is.null(attr(x,"version")} for a missing version attribute could
  turn into \code{is.null(x@version)} for the formal class.

  The requirement that slots have a fixed class can be satisfied
  indirectly as well.  Slots \emph{can} be specified with class
  \code{"ANY"}, allowing an arbitrary object.  However, this eliminates
  an important benefit of formal class definitions; namely, automatic
  validation of objects assigned to a slot.  If just a few different
  classes are possible, consider using \code{\link{setClassUnion}} to
  define valid objects for a slot.
}

\seealso{
  \code{\link{setClass}}, \code{\link{setMethod}}
}

\examples{
setOldClass(c("mlm", "lm"))
setGeneric("dfResidual", function(model)standardGeneric("dfResidual"))
setMethod("dfResidual", "lm", function(model)model$df.residual)

## dfResidual will work on mlm objects as well as lm objects
myData <- data.frame(time = 1:10, y = (1:10)^.5)
myLm <- lm(cbind(y, y^3)  ~ time, myData)

\dontshow{
stopifnot(identical(dfResidual(myLm), myLm$df.residual))}

rm(myData, myLm)
removeGeneric("dfResidual")
}
\keyword{ programming }
\keyword{ methods }
