\name{formatC}
\title{Formatting Using C-style Formats}
\usage{
formatC(x, digits = NULL, width = NULL,
        format = NULL, flag = "", mode = NULL,
        big.mark = "", big.interval = 3,
      small.mark = "", small.interval = 5,
    decimal.mark = ".")

format.char(x, width = NULL, flag = "-")
}
\alias{formatC}
\alias{format.char}
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

  \item{flag}{format modifier as in Kernighan and Ritchie (1988, page 243).
    \code{"0"}  pads leading zeros; \code{"-"} does left adjustment,
    others are \code{"+"}, \code{" "}, and \code{"#"}.  There can be
    more than one of these, in any order.}

  \item{mode}{\code{"double"} (or \code{"real"}), \code{"integer"} or
    \code{"character"}.
    Default: Determined from the storage mode of \code{x}.}

  \item{big.mark, big.interval,
    small.mark, small.interval,
    decimal.mark}{used for prettying longer decimal sequences, passed to
    \code{\link{prettyNum}}: that help page explains the details.}
}
\description{
    Formatting numbers individually and flexibly, using \code{C} style
    format specifications.  \code{format.char} is a helper function for
    \code{formatC}.
}
\value{
  A character object of same size and attributes as \code{x}.
  Unlike \code{\link{format}}, each number is formatted individually.
  Looping over each element of \code{x}, \code{sprintf(\dots)} is
  called (inside the C function \code{str_signif}).

  \code{format.char(x)} and \code{formatC}, for character \code{x}, do
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
}
\author{
  Originally written by Bill Dunlap, later much improved by Martin Maechler,
  it was first adapted for \R by Friedrich Leisch.
}
\references{
  Kernighan, B. W. and Ritchie, D. M. (1988)
  \emph{The C Programming Language.}  Second edition. Prentice Hall.
}
\seealso{
  \code{\link{format}}, \code{\link{sprintf}} for more general C like
  formatting.
}
\examples{
xx  <- pi * 10^(-5:4)
cbind(format(xx, digits=4), formatC(xx))
cbind(formatC(xx, wid = 9, flag = "-"))
cbind(formatC(xx, dig = 5, wid = 8, format = "f", flag = "0"))
cbind(format(xx, digits=4), formatC(xx, dig = 4, format = "fg"))

format.char(c("a", "Abc", "no way"), wid = -7)  # <=> flag = "-"
formatC(    c("a", "Abc", "no way"), wid = -7)  # <=> flag = "-"
formatC(c((-1:1)/0,c(1,100)*pi), wid=8, dig=1)

xx <- c(1e-12,-3.98765e-10,1.45645e-69,1e-70,pi*1e37,3.44e4)
##       1        2             3        4      5       6
formatC(xx)
formatC(xx, format="fg")       # special "fixed" format.
formatC(xx, format="f", dig=80)#>> also long strings
}
\keyword{character}
\keyword{print}
