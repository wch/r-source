\name{setOldClass}
\alias{setOldClass}
\alias{POSIXct-class}
\alias{POSIXt-class}
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
  The \code{Classes} argument is an old-style class assignment.  In
  particular, if there is more than one name, we expect old-style
  class inheritance.  Calling \code{setOldClass} establishes similar
  inheritance for formal method dispatch, so that the class names can
  be used in a call to \code{\link{setMethod}}.
}
\usage{
setOldClass(Classes, where = 1)
}
\arguments{
  \item{Classes}{ A character vector of names for old-style classes. }
    \item{where}{ Where to store the class definitions.}
}
\details{
  Each of the names will be defined as a virtual class, extending the
  remaining classes in \code{Classes}, and the class
  \code{oldClass}, which is the ``root'' of all old-style classes.

  See the list \code{.OldClassesList} for the old-style classes that
  are defined by the methods package.  Each element of the list is an
  old-style list, with multiple character strings if inheritance is
  included.
  Each element of the list was passed to \code{setOldClass} when
  creating the methods package.
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

\testonly{stopifnot(identical(dfResidual(myLm), myLm$df.residual))}

rm(myData, myLm)
removeGeneric("dfResidual")
removeClass("mlm")
removeClass("lm")
}
\keyword{ programming }
\keyword{ methods }
