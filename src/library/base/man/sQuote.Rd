% File src/library/base/man/sQuote.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Development Team
% Distributed under GPL 2 or later

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
  Unicode markup cannot be assumed to be available, it seems good practice
  to use the apostrophe as an undirectional single quotation mark.

  Similarly, Unicode has left and right double quotation mark characters
  (U+201C and U+201D); if only ASCII's typewriter characteristics can be
  employed, than the ASCII quotation mark (0x22) should be used as both
  the left and right double quotation mark.

  Some other locales also have the directional quotation marks, notably
  on Windows.  TeX uses grave and apostrophe for the directional single
  quotation marks, and doubled grave and doubled apostrophe for the
  directional double quotation marks.

  What rendering is used depend on the \code{\link{options}} setting
  for \code{useFancyQuotes}.  If this is \code{FALSE} (the default under
  Windows) then the undirectional ASCII quotation style is used.  If
  this is \code{TRUE} (the default under Unix-alikes), Unicode
  directional quotes are used are used where available: if set to
  \code{"UTF-8"} UTF-8 markup is used (whatever the current locale).  If
  set to \code{"TeX"}, TeX-style markup is used.  Finally, if this is set
  to a character vector of length four, the first two entries are used
  for beginning and ending single quotes and the second two for
  beginning and ending double quotes: this can be used to implement
  non-English quoting conventions such as the use of guillemets.

  Where fancy quotes are used, you should be aware that they may not be
  rendered correctly as not all fonts include the requisite glyphs: for
  example some have directional single quotes but not directional double
  quotes.
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
op <- options("useFancyQuotes")
paste("argument", sQuote("x"), "must be non-zero")
options(useFancyQuotes = FALSE)
cat("\ndistinguish plain", sQuote("single"), "and",
    dQuote("double"), "quotes\n")
options(useFancyQuotes = TRUE)
cat("\ndistinguish fancy", sQuote("single"), "and",
    dQuote("double"), "quotes\n")
options(useFancyQuotes = "TeX")
cat("\ndistinguish TeX", sQuote("single"), "and",
    dQuote("double"), "quotes\n")
if(l10n_info()$`Latin-1`) {
    options(useFancyQuotes = c("\xab", "\xbb", "\xbf", "?"))
    cat("\n", sQuote("guillemet"), "and",
        dQuote("Spanish question"), "style\n")
} else if(l10n_info()$`UTF-8`) {
    options(useFancyQuotes = c("\u00ab", "\u00bb", "\u00bf", "?"))
    cat("\n", sQuote("guillemet"), "and",
        dQuote("Spanish question"), "style\n")
}
options(op)
}
\keyword{character}
