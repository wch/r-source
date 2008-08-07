% File src/library/base/man/Encoding.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2008 R Core Development Team
% Distributed under GPL 2 or later

\name{Encoding}
\encoding{latin1}
\alias{Encoding}
\alias{Encoding<-}
\concept{encoding}
\title{Read or Set the Declared Encodings for a Character Vector}
\description{
  Read or set the declared encodings for a character vector.
}
\usage{
Encoding(x)

Encoding(x) <- value
}
\arguments{
  \item{x}{A character vector.}
  \item{value}{A character vector of positive length.}
}
\details{
  Character strings in \R can be declared to be in
  \code{"latin1"} or \code{"UTF-8"}.  These declarations can be read by
  \code{Encoding}, which will return a character vector of values
  \code{"latin1"}, \code{"UTF-8"} or \code{"unknown"}, or set, when
  \code{value} is recycled as needed and other values are silently
  treated as \code{"unknown"}.

  There are other ways for character strings to acquire a declared
  encoding apart from explicitly setting it.  Functions
  \code{\link{scan}}, \code{\link{read.table}}, \code{\link{readLines}},
  \code{\link{parse}} and  \code{\link{source}} have an \code{encoding}
  argument that is used to declare encodings, \code{\link{iconv}}
  declares encodings from its \code{from} argument, and console input in
  suitable locales is also declared.  \code{\link{intToUtf8}} declares
  its output as \code{"UTF-8"}, and output text connections are marked
  if running it a suitable locale.

  Most character manipulation functions will set the encoding on output
  strings if it was declared on the corresponding input.  These include
  \code{\link{chartr}}, \code{\link{strsplit}}, \code{\link{strtrim}},
  \code{\link{substr}}, \code{\link{tolower}} and \code{\link{toupper}}
  as well as \code{\link{sub}(useBytes = FALSE)} and
  \code{\link{gsub}(useBytes = FALSE)}.  (Also, under some circumstances
  \code{\link{paste}} will set an encoding.)   Note that such functions
  do not \emph{preserve} the encoding, but if they know the input
  encoding and that the string has been successfully re-encoded to the
  current encoding, they mark the output with the latter (if it is
  \code{"latin1"} or \code{"UTF-8"}).

  As from \R 2.7.0 \code{\link{substr}} does preserve the encoding, and
  \code{\link{chartr}}, \code{\link{tolower}} and \code{\link{toupper}}
  preserve UTF-8 encoding on systems with Unicode wide characters. With
  their \code{fixed} and \code{perl} options, \code{\link{strsplit}},
  \code{\link{sub}} and \code{gsub} will give a UTF-8 result if any of
  the inputs are UTF-8.

  As from \R 2.7.1 patched, \code{\link{paste}} and \code{\link{sprintf}}
  return a UTF-8 encoded element is any of the inputs to that element
  are UTF-8.
}
\value{
  A character vector.
}
\examples{
## x is intended to be in latin1
x <- "fa\xE7ile"
Encoding(x)
Encoding(x) <- "latin1"
x
xx <- iconv(x, "latin1", "UTF-8")
Encoding(c(x, xx))
c(x, xx)
}
\keyword{utilities}
\keyword{character}
