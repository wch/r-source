% File src/library/base/man/writeLines.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2011 R Core Team
% Distributed under GPL 2 or later

\name{writeLines}
\alias{writeLines}
\title{Write Lines to a Connection}
\description{
  Write text lines to a connection.
}
\usage{
writeLines(text, con = stdout(), sep = "\n", useBytes = FALSE)
}
\arguments{
  \item{text}{A character vector}
  \item{con}{A \link{connection} object or a character string.}
  \item{sep}{character.  A string to be written to the connection after
    each line of text.}
  \item{useBytes}{logical.  See \sQuote{Details}.}
}
\details{
  If the \code{con} is a character string, the function calls
  \code{\link{file}} to obtain a file connection which is opened for
  the duration of the function call.

  If the connection is open it is written from its current position.
  If it is not open, it is opened for the duration of the call in
  \code{"wt"} mode and then closed again.

  Normally \code{writeLines} is used with a text-mode  connection, and the
  default separator is converted to the normal separator for that
  platform (LF on Unix/Linux, CRLF on Windows).  For more control, open
  a binary connection and specify the precise value you want written to
  the file in \code{sep}.  For even more control, use
  \code{\link{writeChar}} on a binary connection.

  \code{useBytes} is for expert use.  Normally (when false) character
  strings with marked encodings are converted to the current encoding
  before being passed to the connection (which might do further
  re-encoding).  \code{useBytes = TRUE} suppresses the re-encoding of
  marked strings so they are passed byte-by-byte to the connection:
  this can be useful when strings have already been re-encoded by
  e.g. \code{\link{iconv}}.  (It is invoked automatically for strings
  with marked encoding \code{"bytes"}.)
}
\seealso{
  \code{\link{connections}}, \code{\link{writeChar}}, \code{\link{writeBin}},
  \code{\link{readLines}}, \code{\link{cat}}
}

\keyword{file}
\keyword{connection}
