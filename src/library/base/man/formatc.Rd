\name{formatC}
\title{Flexible Formatting}
\usage{
formatC(x, digits = NULL, width = max(0, digits) + 1,
        format = NULL, flag = "", mode = NULL)
}
\alias{formatC}
\arguments{
  \item{x}{an atomic numerical or character object, typically a vector
    of real numbers.}
  \item{digits}{the desired number of digits after the decimal
    point. Default: 2 for integer, 4 for real numbers.  If less than 0,
    the C default of 6 digits is used.}
  \item{width}{the total field width; \code{width < 0} means left
    justify the number in this field (equivalent to \code{flag ="-"}).
    It is possible that the result will be longer than this, but that
    should only happen in reasonable cases.}
  \item{format}{equal to \code{"d"}  (for integers), \code{"f"},
    \code{"e"}, \code{"E"}, \code{"g"}, \code{"G"} (for reals), or
    \code{"s"} (for strings).  \code{"f"} gives numbers in the usual
    ``xxx.xxx'' format; \code{"e"} and \code{"E"} give ``n.dddenn'' or
    ``n.dddEnn'' (scientific format); \code{"g"} and \code{"G"} put
    \code{x[i]} into scientific format only if it saves space to do so.}
  \item{flag}{format modifier as in Kernighan and Ritchie, 2nd ed.,
    page 243.
    \code{"0"}  pads leading zeros; \code{"-"} does left adjustment,
    others are \code{"+"}, \code{" "}, and \code{"#"}.}
  \item{mode}{\code{"real"},  \code{"integer"} or \code{"character"}.
    Default: Automatic.}
}
\value{
  A character object of same size and attributes as \code{x}.
  Unlike \code{format}, each number is formatted individually.  A for
  loop over each element of \code{x}, calling \code{sprintf(\dots)} is
  done in the C function \code{str_signif}.

  For character arguments \code{x}, simple (left or right) padding with white
  space is done.
}
\author{
 Originally written by Bill Dunlap, later much improved by Martin Maechler,
 it was adapted for \R by Friedrich Leisch.
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

formatC(c("a", "Abc", "no way"), wid = -7)  # <=> flag = "-"
}
\keyword{character}
\keyword{print}
