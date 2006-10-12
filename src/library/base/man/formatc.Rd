\name{formatC}
\alias{formatC}
\alias{prettyNum}
\description{
    Formatting numbers individually and flexibly, using \code{C} style
    format specifications.
}
\title{Formatting Using C-style Formats}
\usage{
formatC(x, digits = NULL, width = NULL,
        format = NULL, flag = "", mode = NULL,
        big.mark = "", big.interval = 3,
      small.mark = "", small.interval = 5,
    decimal.mark = ".", preserve.width = "individual")

prettyNum(x, big.mark = "",   big.interval = 3,
          small.mark = "", small.interval = 5,
          decimal.mark = ".",
          preserve.width = c("common", "individual", "none"),
          zero.print = NULL, \dots)
}
\arguments{
  \item{x}{an atomic numerical or character object, typically a vector
    of real numbers.}

  \item{digits}{the desired number of digits after the decimal
    point (\code{format = "f"}) or \emph{significant} digits
    (\code{format = "g"}, \code{= "e"} or \code{= "fg"}).

    Default: 2 for integer, 4 for real numbers.  If less than 0,
    the C default of 6 digits is used.}

  \item{width}{the total field width; if both \code{digits} and
    \code{width} are unspecified, \code{width} defaults to 1,
    otherwise to \code{digits + 1}.  \code{width = 0} will use
    \code{width = digits}, \code{width < 0} means left
    justify the number in this field (equivalent to \code{flag ="-"}).
    If necessary, the result will have more characters than \code{width}.}

  \item{format}{equal to \code{"d"}  (for integers), \code{"f"},
    \code{"e"}, \code{"E"}, \code{"g"}, \code{"G"}, \code{"fg"} (for
    reals), or \code{"s"} (for strings). Default is \code{"d"} for
    integers, \code{"g"} for reals.

    \code{"f"} gives numbers in the usual
    \code{xxx.xxx} format;  \code{"e"} and \code{"E"} give \code{n.ddde+nn} or
    \code{n.dddE+nn} (scientific format); \code{"g"} and \code{"G"} put
    \code{x[i]} into scientific format only if it saves space to do so.

    \code{"fg"} uses fixed format as \code{"f"}, but \code{digits} as
    the minimum number of \emph{significant} digits.  That this can lead
    to quite long result strings, see examples below.  Note that unlike
    \code{\link{signif}} this prints large numbers with
    more significant digits than \code{digits}.}

  \item{flag}{For \code{formatC}, a character string giving a
    format modifier as in Kernighan and Ritchie (1988, page 243).
    \code{"0"}  pads leading zeros; \code{"-"} does left adjustment,
    others are \code{"+"}, \code{" "}, and \code{"#"}.  There can be
    more than one of these, in any order.
  }

  \item{mode}{\code{"double"} (or \code{"real"}), \code{"integer"} or
    \code{"character"}.
    Default: Determined from the storage mode of \code{x}.}

  \item{big.mark}{character; if not empty used as mark between every
    \code{big.interval} decimals \emph{before} (hence \code{big}) the
    decimal point.}
  \item{big.interval}{see \code{big.mark} above; defaults to 3.}
  \item{small.mark}{character; if not empty used as mark between every
    \code{small.interval} decimals \emph{after} (hence \code{small}) the
    decimal point.}
  \item{small.interval}{see \code{small.mark} above; defaults to 5.}
  \item{decimal.mark}{the character to be used to indicate the numeric
    decimal point.}
  \item{preserve.width}{string specifying if the string widths should
    be preserved where possible in those cases where marks
    (\code{big.mark} or \code{small.mark}) are added.  \code{"common"},
    the default, corresponds to \code{\link{format}}-like behavior
    whereas \code{"individual"} is the default in
    \code{formatC()}.}
  \item{zero.print}{logical, character string or \code{NULL} specifying
    if and how \emph{zeros} should be formatted specially.  Useful for
    pretty printing \dQuote{sparse} objects.}
  \item{\dots}{arguments passed to \code{format}.}
}
\value{
  A character object of same size and attributes as \code{x}.
  Unlike \code{\link{format}}, each number is formatted individually.
  Looping over each element of \code{x}, the C function
  \code{sprintf(\dots)} is called (inside the C function \code{str_signif}).

  \code{formatC}: for character \code{x}, do
  simple (left or right) padding with white space.
}
\details{
  If you set \code{format} it overrides the setting of \code{mode}, so
  \code{formatC(123.45, mode="double", format="d")} gives \code{123}.

  The rendering of scientific format is platform-dependent: some systems
  use \code{n.ddde+nnn} or \code{n.dddenn} rather than \code{n.ddde+nn}.

  \code{formatC} does not necessarily align the numbers on the decimal
  point, so \code{formatC(c(6.11, 13.1), digits=2, format="fg")} gives
  \code{c("6.1", " 13")}.  If you want common formatting for several
  numbers, use \code{\link{format}}.

  \code{prettyNum} is the utility function for prettifying \code{x}. If
  \code{x} is not a character, \code{format(x[i], ...)} is applied to
  each element, and then it is left unchanged if all the other arguments
  are at their defaults.  Note that \code{prettyNum(x)} may behave
  unexpectedly if \code{x} is a \code{character} vector not resulting from
  something like \code{format(<number>)}: in particular it assumes that
  a period is a decimal mark.
}
\author{
  \code{formatC} was originally written by Bill Dunlap, later much
  improved by Martin Maechler.
  It was first adapted for \R by Friedrich Leisch.
}
\references{
  Kernighan, B. W. and Ritchie, D. M. (1988)
  \emph{The C Programming Language.}  Second edition. Prentice Hall.
}
\seealso{
  \code{\link{format}}.

  \code{\link{sprintf}} for more general C like formatting.
}
\examples{
xx  <- pi * 10^(-5:4)
cbind(format(xx, digits=4), formatC(xx))
cbind(formatC(xx, wid = 9, flag = "-"))
cbind(formatC(xx, dig = 5, wid = 8, format = "f", flag = "0"))
cbind(format(xx, digits=4), formatC(xx, dig = 4, format = "fg"))

formatC(    c("a", "Abc", "no way"), wid = -7)  # <=> flag = "-"
formatC(c((-1:1)/0,c(1,100)*pi), wid=8, dig=1)

xx <- c(1e-12,-3.98765e-10,1.45645e-69,1e-70,pi*1e37,3.44e4)
##       1        2             3        4      5       6
formatC(xx)
formatC(xx, format="fg")       # special "fixed" format.
formatC(xx, format="f", dig=80)#>> also long strings

r <- c("76491283764.97430", "29.12345678901", "-7.1234", "-100.1","1123")
## American:
prettyNum(r, big.mark = ",")
## Some Europeans:
prettyNum(r, big.mark = "'", decimal.mark = ",")

(dd <- sapply(1:10, function(i)paste((9:0)[1:i],collapse="")))
prettyNum(dd, big.mark="'")

## examples of 'small.mark'
pN <- stats::pnorm(1:7, lower=FALSE)
cbind(format (pN, small.mark = " ", digits = 15))
cbind(formatC(pN, small.mark = " ", digits = 17, format = "f"))

cbind(ff <- format(1.2345 + 10^(0:5), width = 11, big.mark = "'"))
## all with same width (one more than the specified minimum)

## individual formatting to common width:
fc <- formatC(1.234 + 10^(0:8), format="fg", width=11, big.mark = "'")
cbind(fc)
}
\keyword{character}
\keyword{print}
