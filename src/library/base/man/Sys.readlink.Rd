% File src/library/base/man/file.info.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2009-11 R Core Team
% Distributed under GPL 2 or later

\name{Sys.readlink}
\alias{Sys.readlink}
\title{
  Read File Symbolic Links
}
\description{
  Find out if a file path is a symbolic link, and if so what it is
  linked to, \emph{via} the system call \code{readlink}.

  Symbolic links are a Unix concept, not implemented on Windows.
}
\usage{
Sys.readlink(paths)
}
\arguments{
  \item{paths}{character vector of file paths.  Tilde expansion is done:
    see \code{\link{path.expand}}.}
}
\value{
  A character vector of the the same length as \code{paths}.  The
  entries are the path of the file linked to, \code{""} if the path is
  not a symbolic link, and \code{NA} if there is an error (e.g., the
  path does not exist).
}
\seealso{
  \code{\link{file.symlink}}, \code{\link{file.info}}
}
\keyword{file}
