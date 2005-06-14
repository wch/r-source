\name{sQuote}
\alias{sQuote}
\alias{dQuote}
\title{Quote Text}
\description{
  Single or double quote text by combining with appropriate single or
  double left and right quotation marks.
}
\usage{
sQuote(x)
dQuote(x)
}
\arguments{
  \item{x}{an \R object, to be coerced to a character vector.}
}
\details{
  The purpose of the functions is to provide a simple means of markup
  for quoting text to be used in the R output, e.g., in warnings or
  error messages.

  The choice of the appropriate quotation marks depends on both the
  locale and the available character sets.  Older Unix/X11 fonts
  displayed the grave accent (0x60) and the apostrophe (0x27) in a way
  that they could also be used as matching open and close single
  quotation marks.  Using modern fonts, or non-Unix systems, these
  characters no longer produce matching glyphs.  Unicode provides left
  and right single quotation mark characters (U+2018 and U+2019); if
  Unicode cannot be assumed, it seems reasonable to use the apostrophe
  as an undirectional single quotation mark.

  Similarly, Unicode has left and right double quotation mark characters
  (U+201C and U+201D); if only ASCII's typewriter characteristics can be
  employed, than the ASCII quotation mark (0x22) should be used as both
  the left and right double quotation mark.

  By default, \code{sQuote} and \code{dQuote} provide undirectional 
  ASCII quotation style.  In a UTF-8 locale (see
  \code{\link{l10n_info}}), the Unicode directional quotes are used.
}
\seealso{
  \code{Quotes} for quoting \R code.

  \code{\link{shQuote}} for quoting OS commands.
}
\references{
  Markus Kuhn, \dQuote{ASCII and Unicode quotation marks}.
  \url{http://www.cl.cam.ac.uk/~mgk25/ucs/quotes.html}
}
\examples{
paste("argument", sQuote("x"), "must be non-zero")
}
\keyword{character}
