% File src/library/utils/man/type.convert.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2022 R Core Team
% Distributed under GPL 2 or later

\name{type.convert}
\alias{type.convert}
\alias{type.convert.default}
\alias{type.convert.list}
\alias{type.convert.data.frame}
\title{Convert Data to Appropriate Type}
\description{
  Convert a data object to logical, integer, numeric, complex, character
  or factor as appropriate.
}
\usage{
type.convert(x, \dots)
\method{type.convert}{default}(x, na.strings = "NA", as.is, dec = ".",
             numerals = c("allow.loss", "warn.loss", "no.loss"),
             tryLogical = TRUE, \dots)
\method{type.convert}{data.frame}(x, \dots)
\method{type.convert}{list}(x, \dots)
}
\arguments{
  \item{x}{a vector, matrix, array, data frame, or list.}
  \item{na.strings}{a vector of strings which are to be interpreted as
    \code{\link{NA}} values.  Blank fields are also considered to be
    missing values in logical, integer, numeric or complex vectors.}
  \item{as.is}{whether to store strings as plain \code{character}. When
    false, convert character vectors to
    factors.  See \sQuote{Details}.}
  \item{dec}{the character to be assumed for decimal points.}
  \item{numerals}{string indicating how to convert numbers whose
    conversion to double precision would lose accuracy, typically when
    \code{x} has more digits than can be stored in a \code{\link{double}}.
    Can be abbreviated.  Possible values are
    \describe{
      \item{\code{numerals = "allow.loss"}, default:}{the conversion
	happens with some accuracy loss.}
      \item{\code{numerals = "warn.loss"}:}{a \code{\link{warning}}
	about accuracy loss is signalled and the conversion happens as
	with \code{numerals = "allow.loss"}.}
      \item{\code{numerals = "no.loss"}:}{\code{x} is \emph{not}
	converted to a number, but to a \code{\link{factor}} or
	\code{character}, depending on \code{as.is}.}
    }
  }
  \item{tryLogical}{a \code{\link{logical}} determining if vectors
    consisting entirely of \code{F}, \code{T}, \code{FALSE}, \code{TRUE}
    and \code{na.strings} should be converted to \code{\link{logical}};
    true, by default.}
  \item{\dots}{arguments to be passed to or from methods.}
}
\details{
  This helper function is used by \code{\link{read.table}}. When the
  data object \code{x} is a data frame or list, the function is called
  recursively for each column or list element.

  Given a vector, the function attempts to convert it to logical,
  integer, numeric or complex, and when additionally \code{as.is = FALSE}
  (no longer the default!), converts a character vector to
  \code{\link{factor}}.  The first type that can accept all the non-missing
  values is chosen.

  Vectors which are entirely missing values are converted to logical,
  since \code{NA} is primarily logical.

  If \code{tryLogical} is true as by default, vectors containing just
  \code{F}, \code{T}, \code{FALSE}, \code{TRUE}
  and values from \code{na.strings} are converted to logical.  This may be surprising
  in a context where you have many \code{character} columns with e.g.,
  1-letter strings and you happen to get one with only \code{"F"}.  In such
  cases \code{tryLogical = FALSE} is recommended.
  %
  Vectors containing optional whitespace followed by decimal constants
  representable as \R integers or values from \code{na.strings} are
  converted to integer.  Other vectors containing optional whitespace
  followed by other decimal or hexadecimal constants (see
  \link{NumericConstants}), or \code{NaN}, \code{Inf} or \code{infinity}
  (ignoring case) or values from \code{na.strings} are converted to
  numeric.  Where converting inputs to numeric or complex would result
  in loss of accuracy they can optionally be returned as strings or (for
  \code{as.is = FALSE}) factors.

  Since this is a helper function, the caller should always pass an
  appropriate value of \code{as.is}.
}
\value{
  An object like \code{x} but using another storage mode when
  appropriate.
}
\author{R Core, with a contribution by Arni Magnusson}
\seealso{
  \code{\link{read.table}}, \code{\link{class}},
  \code{\link{storage.mode}}.
}
\examples{
## Numeric to integer
class(rivers)
x <- type.convert(rivers, as.is = TRUE)
class(x)

## Convert many columns
auto <- type.convert(mtcars, as.is = TRUE)
str(mtcars)
str(auto)

## Convert matrix
phones <- type.convert(WorldPhones, as.is = TRUE)
storage.mode(WorldPhones)
storage.mode(phones)

## Factor or character
chr <- c("A", "B", "B", "A")
ch2 <- c("F", "F", "NA", "F")
(fac <- factor(chr))
type.convert(chr, as.is = FALSE) # -> factor
type.convert(fac, as.is = FALSE) # -> factor
type.convert(chr, as.is = TRUE)  # -> character
type.convert(fac, as.is = TRUE)  # -> character
type.convert(ch2, as.is = TRUE)                   #-> logical
type.convert(ch2, as.is = TRUE, tryLogical=FALSE) #-> character
}
\keyword{manip}
