\name{unlink}
\title{Delete Files and Directories}
\usage{unlink(x, recursive = FALSE)}
\alias{unlink}
\arguments{
  \item{x}{a character vector with the names of the file(s) or
    directories to be deleted.
    Wildcards (normally \file{*} and \file{?}) are allowed.}
  \item{recursive}{logical. Should directories be deleted
    recursively?}
}
\description{
  \code{unlink} deletes the file(s) or directories specified by \code{x}.
}
\details{
  If \code{recursive = FALSE} directories are not deleted,
  not even empty ones.

  \code{\link{file.remove}} can only remove files, but gives more
  detailed error information.
#ifdef unix

  Wildcard expansion is done by \code{\link{Sys.glob}}.
#endif
}
\value{
#ifdef unix
  The return value of the corresponding system command, \code{rm -f},
  normally \code{0} for success, \code{1} for failure.
#endif
#ifdef windows
  \code{0} for success, \code{1} for failure.
#endif
  Not deleting a non-existent file is not a failure.
}
\note{
  Prior to \R version 1.2.0 the default on Unix was \code{recursive =
    TRUE}, and on Windows empty directories could be deleted.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{file.remove}}.
}
%Examples: tempfile has an 'unlink' example
\keyword{file}
