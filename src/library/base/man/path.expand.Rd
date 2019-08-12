% File src/library/base/man/path.expand.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2019 R Core Team
% Distributed under GPL 2 or later

\name{path.expand}
\alias{path.expand}
\alias{tilde expansion}
\title{Expand File Paths}
\description{
  Expand a path name, for example by replacing a leading tilde by the
  user's home directory (if defined on that platform).
}
\usage{
path.expand(path)
}
\arguments{
  \item{path}{character vector containing one or more path names.}
}
#ifdef unix
\details{
  (These details are for a Unix-alike: the details differ on Windows.)
  
  On most builds of \R a leading \code{~user} will expand to the home
  directory of \code{user}, but not on builds without \code{readline}
  installed.  (In an interactive session \code{capabilities("cledit")}
  will report if \code{readline} is available.)

  The \sQuote{path names} need not exist nor be valid path names but
  they do need to be representable in the session encoding.
}
#endif
#ifdef windows
\details{
  (These details are for Windows: the details differ on Unix.)
  
  The definition of the \sQuote{home} directory is in the \file{rw-FAQ}
  Q2.14: it is taken from the \env{R_USER} environment variable when
  \code{path.expand} is first called in a session.

  The \sQuote{path names} need not exist nor be valid path names.
}
#endif
\value{
  A character vector of possibly expanded path names: where the home
  directory is unknown or none is specified the path is returned unchanged.
}
\seealso{
  \code{\link{basename}}, \code{\link{normalizePath}}.
}
\examples{
path.expand("~/foo")
}
\keyword{file}
