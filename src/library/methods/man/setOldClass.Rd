\name{setOldClass}
\alias{setOldClass}
\title{ Specify Names for Old-Style Classes }
\description{
  The \code{Classes} argument is an old-style class assignment.  In
  particular, if there is more than one name, we expect old-style
  class inheritance.  Calling \code{setOldClass} establishes similar
  inheritance for formal method dispatch, so that the class names can
  be used in a call to \code{\link{setMethod}}.
}
\usage{
setOldClass(Classes)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{Classes}{ A character vector of names for old-style classes. }
}
\details{
 Each of the names will be defined as a virtual class, extending the
 remaining classes in \code{Classes}, and the class
 \code{oldClass}, which is the ``root'' of all old-style classes.
}

\seealso{  \code{\link{setClass}}, \code{\link{setMethod}}  }

\examples{
  setOldClass(c("mlm", "lm"))

  setGeneric("dfResidual", function(model)standardGeneric("dfResidual"))

  setMethod("dfResidual", "lm", function(model)model$df.residual)
 
  ## dfResidual will work on mlm objects as well as lm objects

\testonly{

  myData <- data.frame(time = 1:10, y = cbind((1:10)^.5, (1:10)^1.5))

  myLm <- lm(y ~ time, myData)

  stopifnot(identical(dfResidual(mylm), mylm$df.residual))

  rm(myData, myLm)
  removeGeneric("dfResidual")
  removeClass("lm")
  removeClass("mlm")
}
\keyword{ programming }
\keyword{ methods }
