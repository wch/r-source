\name{Deprecated}
%------ NOTE:  ../R/Deprecated.R   must be synchronized with this!
\title{Deprecated Functions}
\alias{Deprecated}
\alias{.Deprecated}
\usage{
.Deprecated(new)

provide(package)
}
\alias{provide}
\alias{.Provided}
%------ PLEASE: put \alias{.} here for  EACH !
%
\description{
  These functions are provided for compatibility with older versions of
  \R only, and may be defunct as soon as of the next release.

  \code{.Deprecated("<new name>")} is called from deprecated functions.

  \code{provide} allows code to register the names of packages that it
  encompasses.  The argument is stored in the list \code{.Provided}.
  \code{provide} returns \code{FALSE} if the package was already present
  in \code{.Provided} or among the packages in \code{search()}.  The main
  use for \code{provide} is when multiple packages share code.  This is
  most likely when the code implements features present in S(-PLUS) but
  not in \R.

}
\seealso{\code{\link{Defunct}}}
%\examples{
%save.plot # to see how the ``Deprecated'' reference is made automagically
%}
\keyword{documentation}
\keyword{utilities}
