% File src/library/utils/man/type.convert.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2015 R Core Team
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
\method{type.convert}{default}(x, na.strings = "NA", as.is = FALSE, dec = ".",
             numerals = c("allow.loss", "warn.loss", "no.loss"), \dots)
\method{type.convert}{data.frame}(x, \dots)
\method{type.convert}{list}(x, \dots)
}
\arguments{
  \item{x}{a vector, matrix, array, data frame, or list.}
  \item{na.strings}{a vector of strings which are to be interpreted as
    \code{\link{NA}} values.  Blank fields are also considered to be
    missing values in logical, integer, numeric or complex vectors.}
  \item{as.is}{whether to convert factors to character vectors. The
    default is to convert character vectors to factors.  See
    \sQuote{Details}.}
  \item{dec}{the character to be assumed for decimal points.}
  \item{numerals}{string indicating how to convert numbers whose
    conversion to double precision would lose accuracy, typically when
    \code{x} has more digits than can be stored in a \code{\link{double}}.
    Can be abbreviated.  Possible values are
    \describe{
      \item{\code{numerals = "allow.loss"}, default:}{the conversion
	happens with some accuracy loss.  This was the behavior of \R
        versions 3.0.3 and earlier, and the default from 3.1.1 onwards.}
      \item{\code{numerals = "warn.loss"}:}{a \code{\link{warning}}
	about accuracy loss is signalled and the conversion happens as
	with \code{numerals = "allow.loss"}.}
      \item{\code{numerals = "no.loss"}:}{\code{x} is \emph{not}
	converted to a number, but to a \code{\link{factor}} or
	\code{character}, depending on \code{as.is}.  This was the
        behavior of \R version 3.1.0.}
    }
  }
  \item{\dots}{arguments to be passed to or from methods.}
}
\details{
  This helper function is used by \code{\link{read.table}}. When the
  data object \code{x} is a data frame or list, the function is called
  recursively for each column or list element.

  Given a vector, the function attempts to convert it to logical,
  integer, numeric or complex, and failing that converts a character
  vector to factor unless \code{as.is = TRUE}.  The first type that can
  accept all the non-missing values is chosen.

  Vectors which are entirely missing values are converted to logical,
  since \code{NA} is primarily logical.

  Vectors containing just \code{F}, \code{T}, \code{FALSE}, \code{TRUE}
  and values from \code{na.strings} are converted to logical.  Vectors
  containing optional whitespace followed by decimal constants
  representable as \R integers or values from \code{na.strings} are
  converted to integer.  Other vectors containing optional whitespace
  followed by other decimal or hexadecimal constants (see
  \link{NumericConstants}), or \code{NaN}, \code{Inf} or \code{infinity}
  (ignoring case) or values from \code{na.strings} are converted to
  numeric.  Where converting inputs to numeric or complex would result
  in loss of accuracy they can optionally be returned as strings (for
  \code{as.is = TRUE}) or factors.

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
x <- type.convert(rivers)
class(x)

## Convert many columns
auto <- type.convert(mtcars)
str(mtcars)
str(auto)

## Convert matrix
phones <- type.convert(WorldPhones)
storage.mode(WorldPhones)
storage.mode(phones)

## Factor or character
chr <- c("A", "B", "B", "A")
fac <- factor(c("A", "B", "B", "A"))
type.convert(chr)               # -> factor
type.convert(fac)               # -> factor
type.convert(chr, as.is = TRUE) # -> character
type.convert(fac, as.is = TRUE) # -> character
}
\keyword{manip}
