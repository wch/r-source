% File src/library/methods/man/setOldClass.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Development Team
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
\alias{initialize,data.frame-method}
\alias{initialize,factor-method}
\alias{initialize,ordered-method}
\alias{initialize,table-method}
\alias{initialize,summary.table-method}
\title{ Specify Names for Old-Style Classes }
\description{
  Register an old-style (a.k.a. \sQuote{S3}) class as a formally defined
  class. The \code{Classes} argument is the character vector used as the
  \code{class} attribute; in particular, if there is more than one
  string,  old-style class inheritance is mimicked.  Registering via
  \code{setOldClass} allows S3 classes to appear  in method
  signatures, as a slot in an S4 class or as a superclass of an S4 class.
}
\usage{
setOldClass(Classes, prototype, where, test = FALSE)
}
\arguments{
  \item{Classes}{
    A character vector, giving the names for old-style
    classes, as they would appear on the right side of an assignment of
    the \code{class} attribute in S3 computations.
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
  \item{test}{flag, if \code{TRUE}, inheritance must be tested
    explicitly for each object, needed if the S3 class can have a
    different set of class strings, with the same first string.
    See the details below.
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
   the class will be created as a virtual S4
  class.
The main disadvantage is that the prototype object in an S4 class that
uses this class as a slot will have a \code{NULL} object in that slot,
which can sometimes lead to confusion.

 Beginning with version 2.8.0 of \R, support is provided for using a
 (registered) S3 class as a super class of a new S4 class.  See the
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
  are defined by the methods package.  Each element of the list is an
  old-style list, with multiple character strings if inheritance is
  included.
  Each element of the list was passed to \code{setOldClass} when
  creating the \pkg{methods} package; therefore, these classes can be used
  in \code{\link{setMethod}} calls, with the inheritance as implied by
  the list.
}

\section{Extending S3 Classes}{
  A call to \code{setOldClass} creates formal classes corresponding
  to S3 classes, allows these to be used as slots in other classes or in
  a signature in \code{\link{setMethod}}, and mimics the S3
  inheritance.

  In documentation for the initial implementation of S4 classes in \R,
  users were warned against defining S4 classes that contained S3
  classes, even if those had been registered.  The warning was based
  mainly on two points.  1) The S3 behavior of the objects would fail
  because the S3 class would not be visible, for example, when S3
  methods are dispatched.  2) Because S3 classes have no formal
  definition, nothing can be asserted in general about the S3 part of
  an object from such a class.  (The warning was repeated as recently
  as the first reference below.)

  Nevertheless, defining S4 classes to contain an S3 class and extend
  its behavior is attractive in many applications.  The alternative is
  to be stuck with S3 programming, without the flexibility and security of
  formal class and method definitions.

  Beginning with version 2.8.0, \R will provide support for extending
  registered S3 classes; that is, for new classes defined by a call to
  \code{\link{setClass}} in which the \code{contains=} argument
  includes an S3 class.  See the examples below.  The support is aimed
  primarily at providing the S3 class information for all classes that
  extend class \linkS4class{oldClass}, in particular by ensuring that
  all objects from such classes contain the S3 class in a special slot.

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

## two examples extending S3 class "lm", class "xlm"  directly and "ylm" indirectly
setClass("xlm", representation(eps = "numeric"), contains = "lm")
setClass("ylm", representation(header = "character"), contains = "xlm")
ym1 = new("ylm", myLm, header = "Example", eps = 0.)
## for more examples, see ?\link{S3Class}.

\dontshow{
stopifnot(identical(dfResidual(myLm), myLm$df.residual))
removeClass("ylm"); removeClass("xlm")
rm(myData, myLm)
removeGeneric("dfResidual")}
\dontrun{setOldClass("data.frame", prototoype = data.frame())

}
}
\keyword{ programming }
\keyword{ methods }
