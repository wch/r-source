\name{reg.finalizer}
\alias{reg.finalizer}
\title{Finalization of objects}
\description{
  Registers an \R function to be called upon garbage collection of
  object.
}
\usage{
  reg.finalizer(e, f)
}
\arguments{
  \item{e}{Object to finalize. Must be environment or external pointer.}
  \item{f}{Function to call on finalization. Must accept a single argument,
    which will be the object to finalize.}
}
\value{
  \code{NULL}.
}
\note{
  The purpose of this function is mainly to allow objects that refer to
  external items (a temporary file, say) to perform cleanup actions when
  they are no longer referenced from within \R.  This only makes sense
  for objects that are never copied on assignment, hence the restriction
  to environments and external pointers.
}
\seealso{
  \code{\link{gc}} and \code{\link{Memory}} for garbage collection and
  memory management.
}
\examples{
f <- function(e) print("cleaning....")
g <- function(x){ e <- environment(); reg.finalizer(e,f) }
g()
invisible(gc()) # trigger cleanup
}
\keyword{programming}
\keyword{environment}
