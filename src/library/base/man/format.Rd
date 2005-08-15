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

\method{format}{default}(x, trim = FALSE, digits = NULL,
       nsmall = 0, justify = c("left", "right", "centre", "none"),
       width = NULL, na.encode = TRUE,
       big.mark = "",   big.interval = 3,
     small.mark = "", small.interval = 5,
   decimal.mark = ".", \dots)

\method{format}{data.frame}(x, \dots, justify = "none")

\method{format}{factor}(x, \dots)

\method{format}{AsIs}(x, width = 12, \dots)
}
\arguments{
  \item{x}{any \R object (conceptually); typically numeric.}
  \item{trim}{logical; if \code{FALSE}, logical, numeric and complex
    values are right-justified to a common width: if \code{TRUE} the
    leading blanks for justifcation are suppressed.}
  \item{digits}{how many significant digits are to be used for
    numeric and complex \code{x}.  The default, \code{NULL}, uses
    \code{\link{getOption}(digits)}.  This is a suggestion: enough decimal
    places will be used so that the smallest (in magnitude) number has
    this many significant digits, and also to satisfy \code{nsmall}.
    (For the intepretation for complex numbers see \code{\link{signif}}.)}
  \item{nsmall}{number of digits which will always appear to the right of
    the decimal point in formatting real/complex numbers in
    non-scientific formats.  Allowed values are \code{0 <= nsmall <= 20}.}
  \item{justify}{should a \emph{character} vector be left-justified (the
    default), right-justified, centred or left alone.}
  \item{width}{\code{default} method: the \emph{minimum} field width or
    \code{NULL} or \code{0} for no restriction.

    \code{AsIs} method: the \emph{maximum} field width for non-character
    objects.  \code{NULL} corresponds to the default 12.
  }
  \item{na.encode}{logical: should \code{NA} strings be encoded?}
  \item{\dots}{further arguments passed to or from other methods.}

  \item{big.mark, big.interval,
    small.mark, small.interval,
    decimal.mark}{used for prettying longer decimal sequences, passed to
    \code{\link{prettyNum}}: that help page explains the details.}
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
  will be given class \code{\link{"AsIs"}}.

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
  \code{\link{nchar}}, taking double-width characters and the rendering
  of special characters (as escape sequences, including escaping
  backslash: see \code{\link{print.default}}) into account.  Character
  strings are padded with blanks to the display width of the widest.
  (If \code{na.encode = FALSE} missing character strings are not
  included in the width computations and are not encoded.)

  Numeric vectors are encoded with the minimum number of decimal places
  needed to display all the elements to at least the \code{digit}
  significant digits.  However, if all the elements then have trailing
  zeroes, the number of decimal places is reduced until at least one
  element has a non-zero final digit.
}
\value{
  An object of similar structure to \code{x} containing character
  representations of the elements of the first argument \code{x}
  in a common format.

  For numeric or complex \code{x}, dims and dimnames are preserved on
  matrices/arrays and names on vectors: as from \R 2.2.0 no other
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
  Wadsworth \& Brooks/Cole.
}

\seealso{
  \code{\link{format.info}} indicates how an atomic vector would be formatted;
  \code{\link{formatC}}, \code{\link{paste}}, \code{\link{as.character}},
  \code{\link{sprintf}}, \code{\link{print}}, \code{\link{toString}},
  \code{\link{encodeString}}.
}
\examples{
format(1:10)
format(1:10, trim = TRUE)

zz <- data.frame("(row names)"= c("aaaaa", "b"), check.names=FALSE)
format(zz)
format(zz, justify = "left")

## use of nsmall
format(13.7)
format(13.7, nsmall = 3)
format(c(6.0, 13.1), digits = 2)
format(c(6.0, 13.1), digits = 2, nsmall = 1)

## a list
z <- list(a=letters[1:3], b=(-pi+0i)^((-2:2)/2), c=c(1,10,100,1000),
          d=c("a", "longer", "character", "string"))
format(z, digits = 2)
format(z, digits = 2, justify = "left", trim = FALSE)
}
\keyword{character}
\keyword{print}
