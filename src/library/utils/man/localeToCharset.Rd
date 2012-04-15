% File src/library/utils/man/localeToCharset.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2009 R Core Team
% Distributed under GPL 2 or later

\name{localeToCharset}
\alias{localeToCharset}
\title{
  Select a Suitable Encoding Name from a Locale Name
}
\description{
  This functions aims to find a suitable coding for the locale named, by
  default the current locale, and if it is a UTF-8 locale a suitable
  single-byte encoding.
}
\usage{
localeToCharset(locale = Sys.getlocale("LC_CTYPE"))
}
\arguments{
  \item{locale}{character string naming a locale.}
}
\details{
  The operation differs by OS.
#ifdef windows
  A Windows locale is specified like \code{"English_United Kingdom.1252"}.
  The final component gives the codepage, and this defines the encoding.
#endif
#ifdef unix
  Locale names are normally like \code{es_MX.iso88591}.  If final
  component indicates an encoding and it is not \code{utf8} we just need
  to look up the equivalent encoding name.  Otherwise, the language
  (here \code{es}) is used to choose a primary or fallback encoding.
#endif

  In the \code{C} locale the answer will be \code{"ASCII"}.
}
\value{
  A character vector naming an encoding and possibly a fallback
  single-encoding,  \code{NA} if unknown.
}
\note{
  The encoding names are those used by \code{libiconv}, and ought also
  to work with \code{glibc} but maybe not with commercial Unixen.
}
\seealso{
  \code{\link{Sys.getlocale}}, \code{\link{iconv}}.
}
\examples{
localeToCharset()
}
\keyword{ utilities }
