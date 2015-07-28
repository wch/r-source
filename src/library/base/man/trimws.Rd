\name{trimws}
\alias{trimws}
\title{Remove Leading/Trailing Whitespace}
\description{
  Remove leading and/or trailing whitespace from character strings.
}
\usage{
trimws(x, which = c("both", "left", "right"))
}
\arguments{
  \item{x}{a character vector}
  \item{which}{a character string specifying whether to remove both
    leading and trailing whitespace (default), or only leading
    (\code{"left"}) or trailing (\code{"right"}).  Can be abbreviated.}
}
\details{
  For portability, \sQuote{whitespace} is taken as the character class
  \code{[ \\t\\r\\n]} (space, horizontal tab, line feed, carriage return).
}
\examples{
x <- "  Some text. "
x
trimws(x)
trimws(x, "l")
trimws(x, "r")
}
\keyword{character}
