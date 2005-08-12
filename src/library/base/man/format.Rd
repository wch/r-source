\name{format}
\alias{format}
\alias{format.AsIs}
\alias{format.data.frame}
\alias{format.default}
\alias{format.factor}
\alias{prettyNum}
\description{
  Format an \R object for pretty printing..
}
\title{Encode in a Common Format}
\usage{
format(x, \dots)

\method{format}{AsIs}(x, width = 12, \dots)

\method{format}{data.frame}(x, \dots, justify = "none")

\method{format}{default}(x, trim = FALSE, digits = NULL,
       nsmall = 0, justify = c("left", "right", "none"),
       big.mark = "",   big.interval = 3,
     small.mark = "", small.interval = 5,
   decimal.mark = ".", \dots)

\method{format}{factor}(x, \dots)

prettyNum(x, big.mark = "",   big.interval = 3,
           small.mark = "", small.interval = 5,
         decimal.mark = ".", \dots)
}
\arguments{
  \item{x}{any \R object (conceptually); typically numeric.}
  \item{trim}{logical; if \code{FALSE}, logical, numeric and complex
    values are right-justified to a common width: if \code{TRUE} the
    leading blanks are suppressed.}
  \item{digits}{how many significant digits are to be used for
    \code{\link{numeric} x}.  The default, \code{NULL}, uses
    \code{\link{getOption}(digits)}.  This is a suggestion: enough decimal
    places will be used so that the smallest (in magnitude) number has
    this many significant digits. (For the intepretation for complex
    numbers see \code{\link{signif}}.)}
  \item{nsmall}{number of digits which will always appear to the right of
    the decimal point in formatting real/complex numbers in
    non-scientific formats. Allowed values \code{0 <= nsmall <= 20}.}
  \item{justify}{should character vector be left-justified,
    right-justified or left alone.  When justifying, the field width is
    that of the longest string.}
  \item{width}{the returned vector has elements of at most \code{width}.}
  \item{\dots}{further arguments passed to or from other methods.}

  \item{big.mark}{character; if not empty used as mark between every
    \code{big.interval} decimals \emph{before} (hence \code{big}) the
    decimal point.}
  \item{big.interval}{see \code{big.mark} above; defaults to 3.}
  \item{small.mark}{character; if not empty used as mark between every
    \code{small.interval} decimals \emph{after} (hence \code{small}) the
    decimal point.}
  \item{small.interval}{see \code{small.mark} above; defaults to 5.}
  \item{decimal.mark}{the character used to indicate the numeric
    decimal point.}
}
\value{
  A vector (or array) of character strings displaying the elements of 
  the first argument \code{x} in a common format. 
}
\details{
  The value of these functions satisfies \code{length(format*(x, *)) ==
    length(x)}.  The trimming with \code{trim = TRUE} is useful
  when the strings are to be used for plot \code{\link{axis}} annotation.

  \code{format.AsIs} deals with columns of complicated objects that
  have been extracted from a data frame.

  The function \code{\link{formatC}} provides a rather more flexible
  formatting facility for numbers, but does \emph{not} provide a common
  format for several numbers, nor it is platform-independent.

  \code{format.data.frame} formats the data frame column by column,
  applying the appropriate method of \code{format} for each column.

  \code{prettyNum} is the utility function for prettifying \code{x}. If
  \code{x} is not a character, \code{format(x[i], ...)} is applied to
  each element, and then it is left unchanged if all the other arguments
  are at their defaults.  Note that \code{prettyNum(x)} may behave
  unexpectedly if \code{x} is a \code{character} not resulting from
  something like \code{format(<number>)}.
}
\note{
  Currently \code{format} drops trailing zeroes, so \code{format(6.001,
    digits=2)} gives \code{"6"} and \code{format(c(6.0, 13.1),
    digits=2)} gives \code{c(" 6", "13")}.

  Character(s) \code{"} in input strings \code{x} are escaped to \code{\\"}.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}

\seealso{
  \code{\link{format.info}} indicates how something would be formatted;
  \code{\link{formatC}}, \code{\link{paste}}, \code{\link{as.character}},
  \code{\link{sprintf}}, \code{\link{print}}.
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

r <- c("76491283764.97430", "29.12345678901", "-7.1234", "-100.1","1123")
## American:
prettyNum(r, big.mark = ",")
## Some Europeans:
prettyNum(r, big.mark = "'", decimal.mark = ",")

(dd <- sapply(1:10, function(i)paste((9:0)[1:i],collapse="")))
prettyNum(dd, big.mark="'")

pN <- stats::pnorm(1:7, lower=FALSE)
cbind(format (pN, small.mark = " ", digits = 15))
cbind(formatC(pN, small.mark = " ", digits = 17, format = "f"))
}
\keyword{character}
\keyword{print}
