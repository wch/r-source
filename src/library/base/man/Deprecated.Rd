\name{Deprecated}
%------ NOTE:  ../R/Deprecated.R   must be synchronized with this!
\title{Deprecated Functions}
\alias{Deprecated}
\alias{.Deprecated}
\usage{
getenv(x)

.Deprecated(new)
}
%------ PLEASE: put \alias{.} here for  EACH !
%
\alias{getenv}
\alias{dotplot}
\alias{stripplot}
\description{
  These functions are provided for compatibility with older versions of
  \R only, and may be defunct as soon as of the next release.

  \code{.Deprecated("<new name>")} is called from deprecated functions.

  \code{getenv} is a deprecated alternative name for
  \code{\link{Sys.getenv}}.

  \code{dotplot} and \code{stripplot} are deprecated names for
  \code{\link{dotchart}} and \code{\link{stripchart}}, respectively.
}
\seealso{\code{\link{Defunct}}}
%\examples{
%save.plot # to see how the ``Deprecated'' reference is made automagically
%}
\keyword{documentation}
\keyword{utilities}
