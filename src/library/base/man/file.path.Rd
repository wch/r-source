% File src/library/base/man/file.path.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2019 R Core Team
% Distributed under GPL 2 or later

\name{file.path}
\alias{file.path}
\title{Construct Path to File}
\description{
  Construct the path to a file from components in a platform-independent
  way.
}
\usage{
file.path(\dots, fsep = .Platform$file.sep)
}
\arguments{
  \item{\dots}{character vectors.  Long vectors are not supported.}
  \item{fsep}{the path separator to use (assumed to be ASCII).}
}
\details{
  The implementation is designed to be fast (faster than
  \code{\link{paste}}) as this function is used extensively in \R itself.

  It can also be used for environment paths such as \env{PATH} and
  \env{R_LIBS} with \code{fsep = .Platform$path.sep}.

  Trailing path separators are invalid for Windows file paths apart from
  \file{/} and \file{d:/} (although some functions/utilities do accept
  them), so a trailing \code{/} or \code{\\} is removed there.
}
\value{
  A character vector of the arguments concatenated term-by-term and
  separated by \code{fsep} if all arguments have positive length;
  otherwise, an empty character vector (unlike \code{\link{paste}}).

  An element of the result will be marked (see \code{\link{Encoding}} as
  UTF-8 if run in a UTF-8 locale (when marked inputs are converted to
  UTF-8) or if an component of the result is marked as UTF-8, or as
  Latin-1 in a non-Latin-1 locale.
}
\note{
  The components are by default separated by \code{/}
  (not \code{\\}) on Windows.
}
\keyword{file}
