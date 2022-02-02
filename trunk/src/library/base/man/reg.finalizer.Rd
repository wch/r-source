% File src/library/base/man/reg.finalizer.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2017 R Core Team
% Distributed under GPL 2 or later

\name{reg.finalizer}
\alias{reg.finalizer}
\alias{finalizer}
\title{Finalization of Objects}
\description{
  Registers an \R function to be called upon garbage collection of
  object or (optionally) at the end of an \R session.
}
\usage{
  reg.finalizer(e, f, onexit = FALSE)
}
\arguments{
  \item{e}{Object to finalize.  Must be an environment or an external pointer.}
  \item{f}{Function to call on finalization. Must accept a single argument,
    which will be the object to finalize.}
  \item{onexit}{logical: should the finalizer be run if the object is
    still uncollected at the end of the \R session?}
}
\details{
  The main purpose of this function is to allow objects that refer to
  external items (a temporary file, say) to perform cleanup actions when
  they are no longer referenced from within \R.  This only makes sense
  for objects that are never copied on assignment, hence the restriction
  to environments and external pointers.

  \emph{Inter alia}, it provides a way to program code to be run at
  the end of an \R session without manipulating \code{\link{.Last}}.
  For use in a package, it is often a good idea to set a finalizer on an
  object in the namespace: then it will be called at the end of the
  session, or soon after the namespace is unloaded if that is done
  during the session.
}
\value{
  \code{NULL}.
}
\note{
  \R's interpreter is not re-entrant and the finalizer could be run in
  the middle of a computation.  So there are many functions which it is
  potentially unsafe to call from \code{f}: one example which caused
  trouble is \code{\link{options}}.  Finalizers are
  scheduled at garbage collection but only run at a relatively safe time
  thereafter.
}
\seealso{
  \code{\link{gc}} and \code{\link{Memory}} for garbage collection and
  memory management.
}
\examples{
f <- function(e) print("cleaning....")
g <- function(x){ e <- environment(); reg.finalizer(e, f) }
g()
invisible(gc()) # trigger cleanup
}
\keyword{programming}
\keyword{environment}
