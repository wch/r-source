% File src/library/base/man/format.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2014 R Core Team
% Copyright 2003-2013 The R Foundation
% Distributed under GPL 2 or later

\name{format}
\alias{format}
\alias{format.AsIs}
\alias{format.data.frame}
\alias{format.default}
\alias{format.factor}
\description{
  Format an \R object for pretty printing.
}
\title{Encode in a Common Format}
\usage{
format(x, \dots)

\method{format}{default}(x, trim = FALSE, digits = NULL, nsmall = 0L,
       justify = c("left", "right", "centre", "none"),
       width = NULL, na.encode = TRUE, scientific = NA,
       big.mark = "",   big.interval = 3L,
       small.mark = "", small.interval = 5L,
       decimal.mark = ".", zero.print = NULL,
       drop0trailing = FALSE, \dots)

\method{format}{data.frame}(x, \dots, justify = "none")

\method{format}{factor}(x, \dots)

\method{format}{AsIs}(x, width = 12, \dots)
}
\arguments{
  \item{x}{any \R object (conceptually); typically numeric.}
  
  \item{trim}{logical; if \code{FALSE}, logical, numeric and complex
    values are right-justified to a common width: if \code{TRUE} the
    leading blanks for justification are suppressed.}
  
  \item{digits}{how many significant digits are to be used for
    numeric and complex \code{x}.  The default, \code{NULL}, uses
    \code{\link{getOption}("digits")}.  This is a suggestion: enough decimal
    places will be used so that the smallest (in magnitude) number has
    this many significant digits, and also to satisfy \code{nsmall}.
    (For the interpretation for complex numbers see \code{\link{signif}}.)}
  
  \item{nsmall}{the minimum number of digits to the right of the decimal
    point in formatting real/complex numbers in non-scientific formats.
    Allowed values are \code{0 <= nsmall <= 20}.}
  
  \item{justify}{should a \emph{character} vector be left-justified (the
    default), right-justified, centred or left alone.}
  
  \item{width}{\code{default} method: the \emph{minimum} field width or
    \code{NULL} or \code{0} for no restriction.

    \code{AsIs} method: the \emph{maximum} field width for non-character
    objects.  \code{NULL} corresponds to the default \code{12}.
  }
  
  \item{na.encode}{logical: should \code{NA} strings be encoded?  Note
    this only applies to elements of character vectors, not to numerical,
    complex nor logical \code{NA}s, which are always encoded as \code{"NA"}.}
  % because Mr Gorjanc won't read, PR#12318
  
  \item{scientific}{Either a logical specifying whether
    elements of a real or complex vector should be encoded in scientific
    format, or an integer penalty (see \code{\link{options}("scipen")}).
    Missing values correspond to the current default penalty.}
  
  \item{\dots}{further arguments passed to or from other methods.}

  \item{big.mark, big.interval, small.mark,
    small.interval, decimal.mark, zero.print, drop0trailing}{%
    used for prettying (longish) numerical and complex sequences.
    Passed to \code{\link{prettyNum}}: that help page explains the details.}
}
\details{
  \code{format} is a generic function.  Apart from the methods described
  here there are methods for dates (see \code{\link{format.Date}}),
  date-times (see \code{\link{format.POSIXct}})) and for other classes such
  as \code{format.octmode} and \code{format.dist}.

  \code{format.data.frame} formats the data frame column by column,
  applying the appropriate method of \code{format} for each column.
  Methods for columns are often similar to \code{as.character} but offer
  more control.  Matrix and data-frame columns will be converted to
  separate columns in the result, and character columns (normally all)
  will be given class \code{"\link{AsIs}"}.

  \code{format.factor} converts the factor to a character vector and
  then calls the default method (and so \code{justify} applies).

  \code{format.AsIs} deals with columns of complicated objects that
  have been extracted from a data frame.  Character objects are passed
  to the default method (and so \code{width} does not apply).
  Otherwise it calls \code{\link{toString}} to convert the object
  to character (if a vector or list, element by element) and then
  right-justifies the result.

  Justification for character vectors (and objects converted to
  character vectors by their methods) is done on display width (see
  \code{\link{nchar}}), taking double-width characters and the rendering
  of special characters (as escape sequences, including escaping
  backslash but not double quote: see \code{\link{print.default}}) into
  account.  Thus the width is as displayed by \code{print(quote =
  FALSE)} and not as displayed by \code{\link{cat}}.  Character strings
  are padded with blanks to the display width of the widest.  (If
  \code{na.encode = FALSE} missing character strings are not included in
  the width computations and are not encoded.)

  Numeric vectors are encoded with the minimum number of decimal places
  needed to display all the elements to at least the \code{digits}
  significant digits.  However, if all the elements then have trailing
  zeroes, the number of decimal places is reduced until 
  \code{nsmall} is reached or at least one
  element has a non-zero final digit; see also the argument
  documentation for \code{big.*}, \code{small.*} etc, above.  See the
  note in \code{\link{print.default}} about \code{digits >= 16}.

  Raw vectors are converted to their 2-digit hexadecimal representation
  by \code{\link{as.character}}.
  
  The internal code respects the option
  \code{\link{getOption}("OutDec")} for the \sQuote{decimal mark}, so if
  this is set to something other than \code{"."} then it takes precedence
  over argument \code{decimal.mark}.
}
\value{
  An object of similar structure to \code{x} containing character
  representations of the elements of the first argument \code{x}
  in a common format, and in the current locale's encoding.

  For character, numeric, complex or factor \code{x}, dims and dimnames
  are preserved on matrices/arrays and names on vectors: no other
  attributes are copied.

  If \code{x} is a list, the result is a character vector obtained by
  applying \code{format.default(x, \dots)} to each element of the list
  (after \code{\link{unlist}}ing elements which are themselves lists),
  and then collapsing the result for each element with
  \code{paste(collapse = ", ")}.  The defaults in this case are
  \code{trim = TRUE, justify = "none"} since one does not usually want
  alignment in the collapsed strings.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}

\seealso{
  \code{\link{format.info}} indicates how an atomic vector would be
  formatted.

  \code{\link{formatC}}, \code{\link{paste}}, \code{\link{as.character}},
  \code{\link{sprintf}}, \code{\link{print}}, \code{\link{prettyNum}},
  \code{\link{toString}}, \code{\link{encodeString}}.
}
\examples{
format(1:10)
format(1:10, trim = TRUE)

zz <- data.frame("(row names)"= c("aaaaa", "b"), check.names = FALSE)
format(zz)
format(zz, justify = "left")

## use of nsmall
format(13.7)
format(13.7, nsmall = 3)
format(c(6.0, 13.1), digits = 2)
format(c(6.0, 13.1), digits = 2, nsmall = 1)

## use of scientific
format(2^31-1)
format(2^31-1, scientific = TRUE)

## a list
z <- list(a = letters[1:3], b = (-pi+0i)^((-2:2)/2), c = c(1,10,100,1000),
          d = c("a", "longer", "character", "string"),
          q = quote( a + b ), e = expression(1+x))
## can you find the "2" small differences?
(f1 <- format(z, digits = 2))
(f2 <- format(z, digits = 2, justify = "left", trim = FALSE))
f1 == f2 ## 2 FALSE, 4 TRUE
}
\keyword{character}
\keyword{print}
