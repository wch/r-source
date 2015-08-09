% File src/library/base/man/path.expand.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2011 R Core Team
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
  On \emph{some} Unix builds of \R, a leading \code{~user} will expand to
  the home directory of \code{user}, but not on Unix versions without
  \code{readline} installed, nor if \R is invoked with
  \option{--no-readline}.

  In an interactive session \code{capabilities("cledit")} will report if
  \code{readline} is available.
}
#endif
#ifdef windows
\details{
  The definition of the \sQuote{home} directory is in the \file{rw-FAQ}
  Q2.14: it is taken from the \env{R_USER} environment variable when
  \code{path.expand} is first called in a session.
}
#endif
\seealso{
  \code{\link{basename}}, \code{\link{normalizePath}}.
}
\examples{
path.expand("~/foo")
}
\keyword{file}
