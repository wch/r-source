\name{trimws}
\alias{trimws}
\title{Remove Leading/Trailing Whitespace}
\description{
  Remove leading and/or trailing whitespace from character strings.
}
\usage{
trimws(x, which = c("both", "left", "right"), whitespace = "[ \\t\\r\\n]")
}
\arguments{
  \item{x}{a character vector}
  \item{which}{a character string specifying whether to remove both
    leading and trailing whitespace (default), or only leading
    (\code{"left"}) or trailing (\code{"right"}).  Can be abbreviated.}
  \item{whitespace}{a string specifying a regular expression to match
    (one character of) \dQuote{white space}, see Details for
    alternatives to the default.}
}
\details{
  Internally, \code{\link{sub}(re, "", *, perl = TRUE)}, i.e., PCRE
  library regular expressions are used.
  For portability, the default \sQuote{whitespace} is the character class
  \code{[ \\t\\r\\n]} (space, horizontal tab, carriage return,
  newline).  Alternatively, \code{[\\h\\v]} is a good (PCRE)
  generalization to match all Unicode horizontal and vertical white
  space characters, see also \url{https://www.pcre.org}.
}
\examples{
x <- "  Some text. "
x
trimws(x)
trimws(x, "l")
trimws(x, "r")

## Unicode --> need "stronger" 'whitespace' to match all :
tt <- "text with unicode 'non breakable space'."
xu <- paste(" \\t\\v", tt, "\\u00a0 \\n\\r")
(tu <- trimws(xu, whitespace = "[\\\\h\\\\v]"))
stopifnot(identical(tu, tt))
}
\keyword{character}
