\name{base-deprecated}
\alias{base-deprecated}
\alias{loadURL}
\alias{delay}
%------ NOTE:  ../R/Deprecated.R   must be synchronized with this!
\title{Deprecated Functions in Base package}
%------ PLEASE: put \alias{.} here for EACH !
\description{
  These functions are provided for compatibility with older versions of
  \R only, and may be defunct as soon as the next release.
}
\usage{
loadURL(url, envir = parent.frame(), quiet = TRUE, \dots)
delay(x, env = .GlobalEnv)
%% put usage here.
}
\arguments{
  \item{url}{a character string naming a URL.}
  \item{envir}{the environment where the data should be loaded.}
  \item{quiet, \dots}{additional arguments to
    \code{\link{download.file}}.}
  \item{x}{an expression}
  \item{env}{an evaluation environment}
}
\details{
  The original help page for these functions is often
  available at \code{help("oldName-deprecated")} (note the quotes).
  Functions in packages other than the base package are listed in
  \code{help("pkg-deprecated")}.

  \code{loadURL} has been superseded by \code{\link{load}(url())}.  Note
  the comments on that help page: Windows users will need to use
  \code{mode="wb"}.
  
  \code{delay} has been superseded by \code{\link{delayedAssign}}. 
  As of the next release of R, promises should never be visible 
  unevaluated.
}
\seealso{
  \code{\link{Deprecated}}
}
\keyword{misc}
