\name{base-deprecated}
\alias{base-deprecated}
\alias{Sys.putenv}
%------ NOTE:  ../R/Deprecated.R   must be synchronized with this!
\title{Deprecated Functions in Base package}
%------ PLEASE: put \alias{.} here for EACH !
\description{
  These functions are provided for compatibility with older versions of
  \R only, and may be defunct as soon as the next release.
}
\usage{
Sys.putenv(\dots)
}
\arguments{
  \item{\dots}{named arguments with values coercible to a character string.}
}
\details{
  The original help page for these functions is often
  available at \code{help("oldName-deprecated")} (note the quotes).
  Functions in packages other than the base package are listed in
  \code{help("pkg-deprecated")}.

  \code{Sys.putenv} is a deprecated synonym for \code{\link{Sys.setenv}}.
}
\seealso{
  \code{\link{Deprecated}}, \code{\link{base-defunct}}
}
\keyword{misc}
