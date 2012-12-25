% File src/library/tools/man/parse_Rd.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2008-10 R Core Team
% Distributed under GPL 2 or later

\name{parse_Rd}
\alias{parse_Rd}
\alias{print.Rd}
\alias{as.character.Rd}
\title{ Parse an Rd file }
\description{
  This function reads an R documentation (Rd) file and parses it, for
  processing by other functions.
}
\usage{
parse_Rd(file, srcfile = NULL, encoding = "unknown",
         verbose = FALSE, fragment = FALSE, warningCalls = TRUE)
\method{print}{Rd}(x, deparse = FALSE, \dots)
\method{as.character}{Rd}(x, deparse = FALSE, \dots)
}
\arguments{
  \item{file}{A filename or text-mode connection.  At present filenames
    work best.}
  \item{srcfile}{\code{NULL}, or a \code{"srcfile"} object.  See the
    \sQuote{Details} section.}
  \item{encoding}{Encoding to be assumed for input strings.}
  \item{verbose}{Logical indicating whether detailed parsing
    information should be printed.}
  \item{fragment}{Logical indicating whether file represents a complete
    Rd file, or a fragment.}
  \item{warningCalls}{Logical: should parser warnings include the call?}
  \item{x}{An object of class Rd.}
  \item{deparse}{If \code{TRUE}, attempt to reinstate the escape characters
    so that the resulting characters will parse to the same object.}
  \item{\dots}{Further arguments to be passed to or from other methods.}
}
\details{
  This function parses \file{Rd} files according to the specification given
  in \url{http://developer.r-project.org/parseRd.pdf}.

  It generates a warning for each parse error and attempts to continue
  parsing.  In order to continue, it is generally necessary to drop some
  parts of the file, so such warnings should not be ignored.
}
\value{
  \code{parse_Rd} returns an object of class \code{"Rd"}.  The
  internal format of this object is subject to change.  The
  \code{as.character()} and \code{print()} methods defined for the
  class return character vectors and print them, respectively.

  Files without a marked encoding are by default assumed to be in the
  native encoding.  An alternate default can be set using the
  \code{encoding} argument.  All text in files is translated to the
  UTF-8 encoding in the parsed object.
}
\references{ \url{http://developer.r-project.org/parseRd.pdf} }
\author{ Duncan Murdoch }
\seealso{
  \code{\link{Rd2HTML}} for the converters that use the output of
  \code{parse_Rd()}.
}
\keyword{utilities}
\keyword{documentation}
