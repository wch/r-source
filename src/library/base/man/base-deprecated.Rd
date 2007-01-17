\name{base-deprecated}
\alias{base-deprecated}
\alias{unix}
\alias{Sys.putenv}
%------ NOTE:  ../R/Deprecated.R   must be synchronized with this!
\title{Deprecated Functions in Base package}
%------ PLEASE: put \alias{.} here for EACH !
\description{
  These functions are provided for compatibility with older versions of
  \R only, and may be defunct as soon as the next release.
}
\usage{
unix(call, intern = FALSE)

Sys.putenv(\dots)
}
\arguments{
  \item{call}{the system command to be invoked, as a string.}
  \item{intern}{a logical (not \code{NA}) which indicates whether to 
    make the output of the command an \R object.}
  \item{\dots}{named arguments with values coercible to a character string.}
  }
\details{
  The original help page for these functions is often
  available at \code{help("oldName-deprecated")} (note the quotes).
  Functions in packages other than the base package are listed in
  \code{help("pkg-deprecated")}.

  \code{unix} is a \emph{deprecated} (since the 1990s) alternative to
  \code{\link{system}}, available for backwards compatibility.

  \code{Sys.putenv} is a deprecated synonym for \code{\link{Sys.setenv}}.
}
\seealso{
  \code{\link{Deprecated}}, \code{\link{base-defunct}}
}
\keyword{misc}
