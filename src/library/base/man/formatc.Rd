\name{formatC}
\title{Flexible Formatting}
\usage{
formatC(x, digits = NULL, width = NULL,
        format = NULL, flag = "", mode = NULL)
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
    reals), or \code{"s"} (for strings). Default \code{"d"} for
    integers, \code{"g"} for reals.

    \code{"f"} gives numbers in the usual
    ``xxx.xxx'' format;  \code{"e"} and \code{"E"} give ``n.dddenn'' or
    ``n.dddEnn'' (scientific format); \code{"g"} and \code{"G"} put
    \code{x[i]} into scientific format only if it saves space to do so.

    \code{"fg"} uses fixed format as \code{"f"}, but \code{digits} as
    number of \emph{significant} digits.  Note that this can lead to
    quite long result strings, see examples below.}
  \item{flag}{format modifier as in Kernighan and Ritchie, 2nd ed.,
    page 243.
    \code{"0"}  pads leading zeros; \code{"-"} does left adjustment,
    others are \code{"+"}, \code{" "}, and \code{"#"}.}
  \item{mode}{\code{"double"} (or \code{"real"}), \code{"integer"} or
    \code{"character"}.
    Default: Automatic.}
}
\description{
    Formatting numbers individually and flexibly, using \code{C} style
    format specifications.
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
  If you set \code{format} it over-rides the setting of \code{mode}, so
  \code{formatC(123.45, mode="double", format="d")} gives \code{123}.
}
\author{
 Originally written by Bill Dunlap, later much improved by Martin Maechler,
 it was first adapted for \R by Friedrich Leisch.
}
\seealso{
  \code{\link{format}}.
}
\examples{
xx  <- pi * 10^(-5:4)
options(digits = 4)   # only for format
cbind(format(xx), formatC(xx))
cbind(formatC(xx, wid = 9, flag = "-"))
cbind(formatC(xx, dig = 5, wid = 8, format = "f", flag = "0"))

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
