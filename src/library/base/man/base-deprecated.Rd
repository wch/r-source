\name{base-deprecated}
\alias{base-deprecated}
\alias{loadURL}
%------ NOTE:  ../R/Deprecated.R   must be synchronized with this!
\title{Deprecated Functions in Base package}
%------ PLEASE: put \alias{.} here for EACH !
\description{
  These functions are provided for compatibility with older versions of
  \R only, and may be defunct as soon as the next release.
}
\usage{
loadURL(url, envir = parent.frame(), quiet = TRUE, \dots)
%% put usage here.
}
\arguments{
  \item{url}{a character string naming a URL.}
  \item{envir}{the environment where the data should be loaded.}
  \item{quiet, \dots}{additional arguments to
    \code{\link{download.file}}.}
}
\details{
  The original help page for these functions is often
  available at \code{help("oldName-deprecated")} (note the quotes).
  Functions in packages other than the base package are listed in
  \code{help("pkg-deprecated")}.

  \code{loadURL} has been superseded by \code{\link{load}(url())}.  Note
  the comments on that help page: Windows users will need to use
  \code{mode="wb"}.
}
\seealso{
  \code{\link{Deprecated}}
}
\keyword{misc}
