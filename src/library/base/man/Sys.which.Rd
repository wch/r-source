% File src/library/base/man/Sys.which.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2007 R Core Development Team
% Distributed under GPL 2 or later

\name{Sys.which}
\alias{Sys.which}
\title{Find Full Paths of Executables}
\description{
  This is an interface to the system command \code{which}.
}
\usage{
Sys.which(names)
}
\arguments{
  \item{names}{Character vector of names of possible executables.}
}
\details{
  The system command \code{shich} reports on the full names of an
  executable (including an executable script) found on the current path.

#ifdef windows
  An \sQuote{executable} has extension \code{".exe"}, \code{".com"},
  \code{".cmd"} or \code{".bat"}.  Such files need not actually be
  executable, but this is what \code{\link{system}} tries.
#endif
}
\value{
  A character vector of the same length as \code{names}, named by
  \code{names}.  The elements are either the full path to the
  executable/script or \code{""} if no executable of that name was
  found.

#ifdef windows
  The paths will be short paths (8+3 components, no spaces) with
  \code{\\} as the path delimiter.
#endif  
}
\examples{
## the first two are likely to exist everywhere
## texi2dvi exists on most Unix-alikes and under MiKTeX
Sys.which(c("ftp", "ping", "texi2dvi", "this-does-not-exist"))
}
\keyword{ utilities }
