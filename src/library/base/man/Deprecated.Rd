\name{Deprecated}
%------ NOTE:  ../R/Deprecated.R   must be synchronized with this!
\title{Deprecated Functions}
\alias{Deprecated}
\alias{.Deprecated}
%------ PLEASE: put \alias{.} here for EACH !
\alias{machine}
\alias{Platform}
\alias{Machine}
\description{
  These functions are provided for compatibility with older versions of
  \R only, and may be defunct as soon as of the next release.
}
\usage{
.Deprecated(new)
}
\details{
  \code{.Deprecated("<new name>")} is called from deprecated
  functions. The original help page for these functions is often
  available at \code{help("oldName-deprecated")} (note the quotes).

  \code{Machine()} and \code{Platform()} are functions returning the
  variables \code{\link{.Machine}} and \code{\link{.Platform}}
  respectively.
}
\seealso{
  \code{\link{Defunct}},
}
\keyword{documentation}
\keyword{utilities}
