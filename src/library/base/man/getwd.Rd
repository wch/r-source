% File src/library/base/man/getwd.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2019 R Core Team
% Distributed under GPL 2 or later

\name{getwd}
\alias{getwd}
\alias{setwd}
\title{Get or Set Working Directory}
\usage{
getwd()
setwd(dir)
}
\arguments{
  \item{dir}{A character string: \link{tilde expansion} will be done.}
}
\description{
  \code{getwd} returns an absolute filepath representing the current
  working directory of the \R process; \code{setwd(dir)} is used to set
  the working directory to \code{dir}.
}
\details{
  See \link{files} for how file paths with marked encodings are interpreted.
}
\note{
  Note that the return value is said to be \strong{an} absolute
  filepath: there can be more than one representation of the path to a
  directory and on some OSes the value returned can differ after
  changing directories and changing back to the same directory (for
  example if symbolic links have been traversed).
}

\seealso{
  \code{\link{list.files}} for the \emph{contents} of a directory.

  \code{\link{normalizePath}} for a \sQuote{canonical} path name.
}

\value{
  \code{getwd} returns a character string or \code{NULL} if the working
  directory is not available.
  On Windows the path returned will use \code{/} as the path separator
  and be encoded in UTF-8.  The path will not have a trailing \code{/}
  unless it is the root directory (of a drive or share on Windows).

  \code{setwd} returns the current directory before the change,
  invisibly and with the same conventions as \code{getwd}.  It will give
  an error if it does not succeed (including if it is not implemented).
}

\examples{
(WD <- getwd())
if (!is.null(WD)) setwd(WD)
}
\keyword{utilities}
