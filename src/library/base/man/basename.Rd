% File src/library/base/man/basename.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2009 R Core Development Team
% Distributed under GPL 2 or later

\name{basename}
\alias{basename}
\alias{dirname}
\title{Manipulate File Paths}
\description{
  \code{basename} removes all of the path up to and including the last
  path separator (if any).

  \code{dirname} returns the part of the \code{path} up to but
  excluding the last path separator, or \code{"."} if there is no path
  separator.
}
\usage{
basename(path)
dirname(path)
}
\arguments{
  \item{path}{character vector, containing path names.}
}
\details{
  For \code{dirname} tilde expansion is done: see the description of
  \code{\link{path.expand}}.
  
  Trailing path separators are removed before dissecting the path,
  and for \code{dirname} any trailing file separators are removed
  from the result.

  On Windows this will accept either \code{\\} or \code{/} as the path
  separator, but \code{dirname} will return a path using \code{/}
  (except if a network share, when the leading \code{\\\\} will be
  preserved).  Only expect these to be able to handle complete
  paths, and not for example just a share or a drive.
}
\note{
  These are not wrappers for the POSIX system functions of the same
  names: in particular they do \strong{not} have the special handling of
  the path \code{"/"} and of returning \code{"."} for empty strings in
  \code{basename}.
}
\value{
  A character vector of the same length as \code{path}.  A zero-length
  input will give a zero-length output with no error.

  If an element of \code{path} is \code{\link{NA}}, so is the result.
}
\seealso{
  \code{\link{file.path}}, \code{\link{path.expand}}.
}
\examples{
basename(file.path("","p1","p2","p3", c("file1", "file2")))
dirname(file.path("","p1","p2","p3","filename"))
}
\keyword{file}
