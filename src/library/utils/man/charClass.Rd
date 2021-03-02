% File src/library/utils/man/charClass.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2021 R Core Team
% Distributed under GPL 2 or later
\name{charClass}
\alias{charClass}
\title{Character Classification}
\description{
  An interface to the (C99) wide character classsification functions in use.
}
\usage{
charClass(x, class)
}
\arguments{
  \item{x}{An integer vector of Unicode points.}
  \item{class}{A character string, one of those given in the
    \sQuote{Details} section.}
}
\details{
  The classification into character classes is platform-dependent.  The
  classes are determined by internal tables on Windows and (optionally
  but by default) on macOS and AIX.

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
    \item{\code{"punct"}}{Punctuation characters. Some platforms treat all
      non-alphanumeric graphical characters as punctuation.}
    \item{\code{"space"}}{Whitespace, including tabs, form and line
      feeds and carriage returns.  Some OSes include non-breaking
      spaces, some exclude them.}
    \item{\code{"upper"}}{Upper-case alphabetic.}
    \item{\code{"xdigit"}}{Hexadecimal character, one of \code{0-9A-fa-f}.}
  }

  Alphabetic characters contain all lower- and upper-case ones and some
  others (for example, those in \sQuote{title case}).
}
\value{
  A logical vector of the same length as \code{x}.
}
\note{
  Non-ASCII digits are excluded by C99 from the class \code{"digit"}
  and most platforms will have them as alphabetic.
  
  It is an assumption that the system's wide character classification
  functions are coded in Unicode points, but this is known to be true
  for all recent platforms.

  In principle the classification could depend on the locale even on
  one platform, but that seems no longer to be seen.
}
\seealso{
  Character classes are used in \link{regular exression}s.
  
  The OS's \command{man} pages for \code{iswctype} and \code{wctype}.
}
\examples{
x <- c(32,48:70)
cl <- c("alnum", "alpha", "blank", "digit", "graph", "punct", "upper", "xdigit")
X <- lapply(cl, function(y) charClass(x,y)); names(X) <- cl
X <- as.data.frame(X); row.names(X) <- intToUtf8(x, multiple=TRUE)
X

## non-breaking space
charClass(0xac, "space")
charClass(0xac, "punct")

## How many printable characters are there? (ca 250,000, differs by OS)
## There are 2^21 - 1 possible Unicode points, most not yet assigned.
pr <- charClass(1:0x1fffff, "print") 
table(pr)
}
