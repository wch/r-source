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
  setOldClass(c("pframe", "data.frame"))

  setMethod("show", "data.frame", function(object)print.data.frame(object))

  ## show(x) will now dispatch on this method for an object of class "pframe"
}
\keyword{ programming }
\keyword{ methods }
