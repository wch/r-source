\name{iconv}
\alias{iconv}
\alias{iconvlist}
\title{Convert Character Vector between Encodings}
\description{
  This uses system facilities to convert a character vector between
  encodings: the \sQuote{i} stands for \sQuote{internationalization}.
}
\usage{
iconv(x, from, to, sub=NA)

iconvlist()
}

\arguments{
  \item{x}{A character vector.}
  \item{from}{A character string describing the current encoding.}
  \item{to}{A character string describing the target encoding.}
  \item{sub}{character string.  If not \code{NA} it is used to replace
    an non-convertible bytes in the input.  (This would normally be a
    single character, but can be more.  If "byte", the indication is
    "<xx>" with the hex code of the byte.}
}

\details{
  The names of encodings and which ones are available (and indeed, if
  any are) is platform-dependent.  On most systems you can use \code{""}
  for the encoding of the current locale.

#ifdef unix
  On many platforms \code{iconvlist} provides an alphabetical list of
  the supported encodings.  On others, the information is on the man
  page for \code{iconv(5)} or elsewhere in the man pages (and beware
  that the system command \code{iconv} may not support the same set of
  encodings as the C functions \R calls).
  Unfortunately, the names are rarely common across platforms.
#endif
#ifdef windows
  \code{iconvlist} provides an alphabetical list of the supported encodings.
#endif
  
  Elements which cannot be converted (perhaps because they are invalid
  or because they cannot be represented in the target encoding)
  will be returned as \code{NA}.
}
\value{
  A character vector of the same length and the same attributes as \code{x}.
}
\note{
  Not all platforms support these functions.  See also
  \code{\link{capabilities}("iconv")}.

#ifdef windows
  The support DLL for these functions is not included in the miniR
  distribution.
#endif
}
\seealso{
  \code{\link{localeToCharset}}, \code{\link{file}}.
}
\examples{\dontrun{
iconvlist()

## convert from Latin-2 to UTF-8: two of the GNU iconv variants.
iconv(x, "ISO_8859-2", "UTF-8")  # 8859-2 on Solaris
iconv(x, "LATIN2", "UTF-8")

(x <- "fa\xE7ile")
charToRaw(xx <- iconv(x, "latin1", "UTF-8"))

iconv(x, "latin1", "ASCII")          #   NA
iconv(x, "latin1", "ASCII", "?")     # "fa?ile"
iconv(x, "latin1", "ASCII", "")      # "faile"
iconv(x, "latin1", "ASCII", "byte")  # "fa<e7>ile"
}}
\keyword{ character }
\keyword{ utilities }
