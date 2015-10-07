% File src/library/utils/man/type.convert.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2015 R Core Team
% Distributed under GPL 2 or later

\name{type.convert}
\alias{type.convert}
\title{Type Conversion on Character Variables}
\description{
  Convert a character vector to logical, integer, numeric, complex or
  factor as appropriate.
}
\usage{
type.convert(x, na.strings = "NA", as.is = FALSE, dec = ".",
             numerals = c("allow.loss", "warn.loss", "no.loss"))
}
\arguments{
  \item{x}{a character vector.}

  \item{na.strings}{a vector of strings which are to be interpreted as
    \code{\link{NA}} values.  Blank fields are also considered to be
    missing values in logical, integer, numeric or complex vectors.}
  \item{as.is}{logical.  See \sQuote{Details}.}
  \item{dec}{the character to be assumed for decimal points.}
  \item{numerals}{string indicating how to convert numbers whose
    conversion to double precision would lose accuracy, typically when
    \code{x} has more digits than can be stored in a \code{\link{double}}.
    Can be abbreviated.  Possible values are
    \describe{
      \item{\code{numerals = "allow.loss"}, default:}{the conversion
	happens with some accuracy loss.  This has been the only
	behavior of \R versions 3.0.3 and earlier.}
      \item{\code{numerals = "warn.loss"}:}{a \code{\link{warning}}
	about accuracy loss is signalled and the conversion happens as
	with \code{numerals = "allow.loss"}.}
      \item{\code{numerals = "no.loss"}:}{\code{x} is \emph{not}
	converted to a number, but to a \code{\link{factor}} or left as
	character, depending on \code{as.is}.  This has been the only
	behavior of \R version 3.1.0.}
    }
  }
}
\details{
  This is principally a helper function for \code{\link{read.table}}.
  Given a character vector, it attempts to convert it to logical,
  integer, numeric or complex, and failing that converts it to factor
  unless \code{as.is = TRUE}.  The first type that can accept all the
  non-missing values is chosen.

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
  An atomic vector or (for \code{as.is = FALSE}) a factor.
}

\seealso{\code{\link{read.table}}}

\keyword{manip}
