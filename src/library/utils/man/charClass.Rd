% File src/library/utils/man/charClass.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2022 R Core Team
% Distributed under GPL 2 or later

\name{charClass}
\alias{charClass}
\title{Character Classification}
\description{
  An interface to the (C99) wide character classification functions in use.
}
\usage{
charClass(x, class)
}
\arguments{
  \item{x}{\strong{Either} a UTF-8-encoded length-1 character vector
    \strong{or} an integer vector of Unicode points (or a vector
    coercible to integer).}
  \item{class}{A character string, one of those given in the
    \sQuote{Details} section.}
}
\details{
  The classification into character classes is platform-dependent.  The
  classes are determined by internal tables on Windows and (optionally
  but by default) on macOS and \I{AIX}.

  The character classes are interpreted as follows:
  \describe{
    \item{\code{"alnum"}}{Alphabetic or numeric.}
    \item{\code{"alpha"}}{Alphabetic.}
    \item{\code{"blank"}}{Space or tab.}
    \item{\code{"cntrl"}}{Control characters.}
    \item{\code{"digit"}}{Digits \code{0-9}.}
    \item{\code{"graph"}}{Graphical characters (printable characters
      except whitespace).}
    \item{\code{"lower"}}{Lower-case alphabetic.}
    \item{\code{"print"}}{Printable characters.}
    \item{\code{"punct"}}{Punctuation characters.  Some platforms treat all
      non-alphanumeric graphical characters as punctuation.}
    \item{\code{"space"}}{Whitespace, including tabs, form and line
      feeds and carriage returns.  Some OSes include non-breaking
      spaces, some exclude them.}
    \item{\code{"upper"}}{Upper-case alphabetic.}
    \item{\code{"xdigit"}}{Hexadecimal character, one of \code{0-9A-fa-f}.}
  }

  Alphabetic characters contain all lower- and upper-case ones and some
  others (for example, those in \sQuote{title case}).

  Whether a character is printable is used to decide whether to escape
  it when printing -- see the help for \code{\link{print.default}}.

  If \code{x} is a character string it should either be ASCII or declared
  as UTF-8 -- see \code{\link{Encoding}}.

  \code{charClass} was added in \R 4.1.0.  A less direct way to examine
  character classes which also worked in earlier versions is to use
  something like \code{grepl("[[:print:]]", intToUtf8(x))} -- however,
  the regular-expression code might not use the same classification
  functions as printing and on macOS used not to.
}
\value{
  A logical vector of the length the number of characters or integers in
  \code{x}.
}
\note{
  Non-ASCII digits are excluded by the C99 standard from the class
  \code{"digit"}: most platforms will have them as alphabetic.
  
  It is an assumption that the system's wide character classification
  functions are coded in Unicode points, but this is known to be true
  for all recent platforms.

  The classification may depend on the locale even on one platform.
}
\seealso{
  Character classes are used in \link{regular expression}s.
  
  The OS's \command{man} pages for \code{iswctype} and \code{wctype}.
}
\examples{
x <- c(48:70, 32, 0xa0) # Last is non-breaking space
cl <- c("alnum", "alpha", "blank", "digit", "graph", "punct", "upper", "xdigit")
X <- lapply(cl, function(y) charClass(x,y)); names(X) <- cl
X <- as.data.frame(X); row.names(X) <- sQuote(intToUtf8(x, multiple = TRUE))
X

charClass("ABC123", "alpha")
## Some accented capital Greek characters
(x <- "\u0386\u0388\u0389")
charClass(x, "upper")

## How many printable characters are there? (Around 280,000 in Unicode 13.)
## There are 2^21-1 possible Unicode points (most not yet assigned).
pr <- charClass(1:0x1fffff, "print") 
table(pr)
}
