\name{iconv}
\alias{iconv}
\title{Convert Character Vector between Encodings}
\description{
  This uses system facilities to convert a character vector between
  encodings: the \sQuote{i} stands for \sQuote{internationalization}.
}
\usage{
iconv(x, from, to)
}

\arguments{
  \item{x}{A character vector.}
  \item{from}{A character string describing the current encoding.}
  \item{to}{A character string describing the target encoding.}
}
\details{
  The names of encodings and which ones are available (and indeed, if
  any are) is platform-dependent.

  On many platforms \code{iconv --list} at a shell prompt will give a
  list of encodings.

  Elements which cannot be converted (perhaps because they are invalid)
  will be returned as \code{NA}.  Converted strings are currently
  limited to 1000 bytes.
}
\value{
  A character vector of the same length and the same attributes as \code{x}.
}
\examples{\dontrun{
## convert from Latin-2 to UTF-8: two of the GNU iconv variants.
iconv(x, "ISO_8859-2", "UTF-8")
iconv(x, "LATIN2", "UTF-8")
}}
\keyword{ character }
\keyword{ utilities }
