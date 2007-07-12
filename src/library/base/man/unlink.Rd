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

  Wildcard expansion is done by the internal code of
  \code{\link{Sys.glob}}.
#ifdef unix
  Wildcards will only be expanded if the system supports it.

  \code{recursive = TRUE} is not supported on all platforms, and may be
  ignored, with a warning.
#endif
#ifdef Windows

  Windows cannot remove the current working directory, nor any file
  which is open nor any directory containing such a file.
#endif
}
\value{
  \code{0} for success, \code{1} for failure.
  Not deleting a non-existent file is not a failure, nor is being unable
  to delete a directory if \code{recursive = FALSE}.
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
