\name{iconv}
\alias{iconv}
\alias{iconvlist}
\title{Convert Character Vector between Encodings}
\description{
  This uses system facilities to convert a character vector between
  encodings: the \sQuote{i} stands for \sQuote{internationalization}.
}
\usage{
iconv(x, from, to)

iconvlist()
}

\arguments{
  \item{x}{A character vector.}
  \item{from}{A character string describing the current encoding.}
  \item{to}{A character string describing the target encoding.}
}

\details{
  The names of encodings and which ones are available (and indeed, if
  any are) is platform-dependent.  Use \code{""} for the encoding of the
  current locale.

#ifdef unix
  On many platforms running \code{iconv --list} at a shell prompt will
  give a list of encodings.  So may the \R function \code{iconvlist}
  (at least on platforms using GNU \code{libiconv}).
#endif
#ifdef windows
  \code{iconvlist} provides an alphabetical list of the supported encodings.
#endif
  
  Elements which cannot be converted (perhaps because they are invalid
  or because they cannot be represented in the target encoding)
  will be returned as \code{NA}.  Converted strings are currently
  limited to 1000 bytes.
}
\value{
  A character vector of the same length and the same attributes as \code{x}.
}
\note{
  Note all platforms support these functions.  See also
  \code{\link{capabilities}("iconv")}.
}
\examples{\dontrun{
iconvlist()

## convert from Latin-2 to UTF-8: two of the GNU iconv variants.
iconv(x, "ISO_8859-2", "UTF-8")
iconv(x, "LATIN2", "UTF-8")
}}
\keyword{ character }
\keyword{ utilities }
