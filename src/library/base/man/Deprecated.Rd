\name{Deprecated}
%------ NOTE:  ../R/Deprecated.R   must be synchronized with this!
\title{Deprecated Functions}
\alias{Deprecated}
\alias{.Deprecated}
%------ PLEASE: put \alias{.} here for EACH !
\alias{read.table.url}
\alias{scan.url}
\alias{source.url}
\alias{httpclient}
\alias{parse.dcf}
\description{
  These functions are provided for compatibility with older versions of
  \R only, and may be defunct as soon as of the next release.
}
\usage{
.Deprecated(new)

read.table.url(url, method,\dots)
scan.url(url, file = tempfile(), method, \dots)
source.url(url, file = tempfile(), method, \dots)
httpclient(url, port=80, error.is.fatal=TRUE, check.MIME.type=TRUE,
           file=tempfile(), drop.ctrl.z=TRUE)
parse.dcf(text = NULL, file = "", fields = NULL, versionfix = FALSE)
}
\details{
  \code{.Deprecated("<new name>")} is called from deprecated functions.

  \code{*.url} are replaced by calling \code{read.table}, \code{scan} or
  \code{source} on a \code{\link{url}} connection.

  \code{httpclient} is used by the deprecated \code{"socket"} method
  of \code{\link{download.file}}.

  \code{parse.dcf} has been replaced by \code{read.dcf}, which is much
  faster, but has a slightly different interface.
}
\seealso{\code{\link{Defunct}}}
\keyword{documentation}
\keyword{utilities}
