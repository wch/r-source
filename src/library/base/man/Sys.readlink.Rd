% File src/library/base/man/file.info.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2009-2018 R Core Team
% Distributed under GPL 2 or later

\name{Sys.readlink}
\alias{Sys.readlink}
\title{
  Read File Symbolic Links
}
\description{
  Find out if a file path is a symbolic link, and if so what it is
  linked to, \emph{via} the system call \code{readlink}.

  Symbolic links are a POSIX concept, not implemented on Windows but for
  most filesystems on Unix-alikes.
}
\usage{
Sys.readlink(paths)
}
\arguments{
  \item{paths}{character vector of file paths.  Tilde expansion is done:
    see \code{\link{path.expand}}.}
}
\value{
  A character vector of the same length as \code{paths}.  The
  entries are the path of the file linked to, \code{""} if the path is
  not a symbolic link, and \code{NA} if there is an error (e.g., the
  path does not exist).

  On platforms without the \code{readlink} system call, all elements are
  \code{""}.
}
\seealso{
  \code{\link{file.symlink}} for the creation of symbolic links (and
  their Windows analogues), \code{\link{file.info}}
}
\examples{
##' To check if files (incl. directories) are symbolic links:
is.symlink <- function(paths) isTRUE(nzchar(Sys.readlink(paths), keepNA=TRUE))
## will return all FALSE when the platform has no `readlink` system call.
is.symlink("/foo/bar")
}
\keyword{file}
