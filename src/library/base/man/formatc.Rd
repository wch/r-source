% File src/library/base/man/formatc.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2019 R Core Team
% Distributed under GPL 2 or later

\name{formatC}
\title{Formatting Using C-style Formats}
\alias{formatC}
\alias{prettyNum}
\alias{.format.zeros}
\description{
  \code{formatC()} formats numbers individually and flexibly using
  \code{C} style format specifications.

  \code{prettyNum()} is used for \dQuote{prettifying} (possibly
  formatted) numbers, also in \code{\link{format.default}}.

  \code{.format.zeros(x)}, an auxiliary function of \code{prettyNum()},
  re-formats the zeros in a vector \code{x} of formatted numbers.
}
\usage{
formatC(x, digits = NULL, width = NULL,
        format = NULL, flag = "", mode = NULL,
        big.mark = "", big.interval = 3L,
        small.mark = "", small.interval = 5L,
        decimal.mark = getOption("OutDec"),
        preserve.width = "individual",
        zero.print = NULL, replace.zero = TRUE,
        drop0trailing = FALSE)

prettyNum(x, big.mark = "",   big.interval = 3L,
          small.mark  = "", small.interval = 5L,
          decimal.mark = getOption("OutDec"), input.d.mark = decimal.mark,
          preserve.width = c("common", "individual", "none"),
          zero.print = NULL, replace.zero = FALSE,
          drop0trailing = FALSE, is.cmplx = NA,
          \dots)

.format.zeros(x, zero.print, nx = suppressWarnings(as.numeric(x)),
              replace = FALSE, warn.non.fitting = TRUE)
}
\arguments{
  \item{x}{an atomic numerical or character object, possibly
    \code{\link{complex}} only for \code{prettyNum()}, typically a
    vector of real numbers.  Any class is discarded, with a warning.}

  \item{digits}{the desired number of digits after the decimal
    point (\code{format = "f"}) or \emph{significant} digits
    (\code{format = "g"}, \code{= "e"} or \code{= "fg"}).

    Default: 2 for integer, 4 for real numbers.  If less than 0,
    the C default of 6 digits is used.  If specified as more than 50, 50
    will be used with a warning unless \code{format = "f"} where it is
    limited to typically 324. (Not more than 15--21 digits need be
    accurate, depending on the OS and compiler used.  This limit is
    just a precaution against segfaults in the underlying C runtime.)
  }

  \item{width}{the total field width; if both \code{digits} and
    \code{width} are unspecified, \code{width} defaults to 1,
    otherwise to \code{digits + 1}.  \code{width = 0} will use
    \code{width = digits}, \code{width < 0} means left
    justify the number in this field (equivalent to \code{flag = "-"}).
    If necessary, the result will have more characters than
    \code{width}.  For character data this is interpreted in characters
    (not bytes nor display width).
  }

  \item{format}{equal to \code{"d"}  (for integers), \code{"f"},
    \code{"e"}, \code{"E"}, \code{"g"}, \code{"G"}, \code{"fg"} (for
    reals), or \code{"s"} (for strings).  Default is \code{"d"} for
    integers, \code{"g"} for reals.

    \code{"f"} gives numbers in the usual
    \code{xxx.xxx} format;  \code{"e"} and \code{"E"} give \code{n.ddde+nn} or
    \code{n.dddE+nn} (scientific format); \code{"g"} and \code{"G"} put
    \code{x[i]} into scientific format only if it saves space to do so
    \emph{and} drop trailing zeros and decimal point - unless \code{flag}
    contains \code{"#"} which keeps trailing zeros for the \code{"g", "G"}
    formats.

    \code{"fg"} (our own hybrid format) uses fixed format as \code{"f"},
    but \code{digits} as the minimum number of \emph{significant} digits.
    This can lead to quite long result strings, see examples below.  Note
    that unlike \code{\link{signif}} this prints large numbers with
    more significant digits than \code{digits}.  Trailing zeros are
    \emph{dropped} in this format, unless \code{flag} contains
    \code{"#"}.}

  \item{flag}{for \code{formatC}, a character string giving a
    format modifier as in Kernighan and Ritchie (1988, page 243) or the
    C+99 standard.\describe{
      \item{\code{"0"}}{pads leading zeros;}
      \item{\code{"-"}}{does left adjustment,}
      \item{\code{"+"}}{ensures a sign in all cases, i.e., \code{"+"} for
	positive numbers ,}
      \item{\code{" "}}{if the first character is not a sign, the space
	character \code{" "} will be used instead.}
      \item{\code{"#"}}{specifies \dQuote{an alternative output form},
	specifically depending on \code{format}.}
      \item{\code{"'"}}{on some platform--locale combination, activates
	\dQuote{thousands' grouping} for decimal conversion,}
      \item{\code{"I"}}{in some versions of \file{glibc} allow for integer
	conversion to use the locale's alternative output digits, if any.}
    }
    There can be more than one of these flags, in any order.  Other characters
    used to have no effect for \code{character} formatting, but signal
    an error since \R 3.4.0.
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
  \item{input.d.mark}{if \code{x} is \code{\link{character}}, the
    character known to have been used as the numeric decimal point in
    \code{x}.}
  \item{preserve.width}{string specifying if the string widths should
    be preserved where possible in those cases where marks
    (\code{big.mark} or \code{small.mark}) are added.  \code{"common"},
    the default, corresponds to \code{\link{format}}-like behavior
    whereas \code{"individual"} is the default in
    \code{formatC()}. Value can be abbreviated.}
  \item{zero.print}{logical, character string or \code{NULL} specifying
    if and how \emph{zeros} should be formatted specially.  Useful for
    pretty printing \sQuote{sparse} objects.}
  \item{replace.zero, replace}{logical; if \code{zero.print} is a
    character string, indicates if the exact zero entries in \code{x}
    should be simply replaced by \code{zero.print}.  Otherwise,
    depending on the widths of the respective strings, the (formatted)
    zeroes are \emph{partly} replaced by \code{zero.print} and then
    padded with \code{" "} to the right were applicable.  In that case
    (false \code{replace[.zero]}), if the \code{zero.print} string does
    not fit, a warning is produced (if \code{warn.non.fitting} is true).

    This works via \code{prettyNum()}, which calls \code{.format.zeros(*,
    replace=replace.zero)} three times in this case, see the \sQuote{Details}.
  }
  \item{warn.non.fitting}{logical; if it is true, \code{replace[.zero]} is
    false and the \code{zero.print} string does not fit, a
    \code{\link{warning}} is signalled.}
  \item{drop0trailing}{logical, indicating if trailing zeros,
    i.e., \code{"0"} \emph{after} the decimal mark, should be removed;
    also drops \code{"e+00"} in exponential formats.  This is simply passed
    to \code{prettyNum()}, see the \sQuote{Details}.}
  \item{is.cmplx}{optional logical, to be used when \code{x} is
    \code{"\link{character}"} to indicate if it stems from
    \code{\link{complex}} vector or not.  By default (\code{NA}),
    \code{x} is checked to \sQuote{look like} complex.}
  \item{\dots}{arguments passed to \code{format}.}

  \item{nx}{numeric vector of the same length as \code{x}, typically the
    numbers of which the character vector \code{x} is the pre-format.}
}
\value{
  A character object of same size and attributes as \code{x} (after
  discarding any class), in the current locale's encoding.

  Unlike \code{\link{format}}, each number is formatted individually.
  Looping over each element of \code{x}, the C function
  \code{sprintf(\dots)} is called for numeric inputs (inside the C
  function \code{str_signif}).

  \code{formatC}: for character \code{x}, do simple (left or right)
  padding with white space.
}
\details{
  For numbers, \code{formatC()} calls \code{prettyNum()} when needed
  which itself calls \code{.format.zeros(*, replace=replace.zero)}.
  (\emph{\dQuote{when needed}}: when \code{zero.print} is not
  \code{NULL}, \code{drop0trailing} is true, or one of \code{big.mark},
  \code{small.mark}, or \code{decimal.mark} is not at default.)

  If you set \code{format} it overrides the setting of \code{mode}, so
  \code{formatC(123.45, mode = "double", format = "d")} gives \code{123}.

  The rendering of scientific format is platform-dependent: some systems
  use \code{n.ddde+nnn} or \code{n.dddenn} rather than \code{n.ddde+nn}.

  \code{formatC} does not necessarily align the numbers on the decimal
  point, so \code{formatC(c(6.11, 13.1), digits = 2, format = "fg")} gives
  \code{c("6.1", " 13")}.  If you want common formatting for several
  numbers, use \code{\link{format}}.

  \code{prettyNum} is the utility function for prettifying \code{x}.
  \code{x} can be complex (or \code{\link{format}(<complex>)}), here.  If
  \code{x} is not a character, \code{format(x[i], ...)} is applied to
  each element, and then it is left unchanged if all the other arguments
  are at their defaults.  Use the \code{input.d.mark} argument for
  \code{prettyNum(x)} when \code{x} is a \code{character} vector not
  resulting from something like \code{format(<number>)} with a period as
  decimal mark.

  Because \code{\link{gsub}} is used to insert the \code{big.mark}
  and \code{small.mark}, special characters need escaping.  In particular,
  to insert a single backslash, use \code{"\\\\\\\\"}.

  The C doubles used for \R numerical vectors have signed zeros, which
  \code{formatC} may output as \code{-0}, \code{-0.000} \dots.

  There is a warning if \code{big.mark} and \code{decimal.mark} are the
  same: that would be confusing to those reading the output.
}
\note{
  The default for \code{decimal.mark} in \code{formatC()} was changed in
  \R 3.2.0: for use within \code{\link{print}} methods in packages which might
  be used with earlier versions: use \code{decimal.mark = getOption("OutDec")}
  explicitly.
}
\author{
  \code{formatC} was originally written by Bill Dunlap for S-PLUS, later
  much improved by Martin Maechler.

  It was first adapted for \R by Friedrich Leisch and since much
  improved by the R Core team.

  %% The utilities \code{prettyNum()} and \code{.format.zeros()} are by
  %% Martin Maechler
}
\references{
  Kernighan, B. W. and Ritchie, D. M. (1988)
  \emph{The C Programming Language.}  Second edition.  Prentice Hall.
}
\seealso{
  \code{\link{format}}.

  \code{\link{sprintf}} for more general C-like formatting.
}
\examples{
xx <- pi * 10^(-5:4)
cbind(format(xx, digits = 4), formatC(xx))
cbind(formatC(xx, width = 9, flag = "-"))
cbind(formatC(xx, digits = 5, width = 8, format = "f", flag = "0"))
cbind(format(xx, digits = 4), formatC(xx, digits = 4, format = "fg"))

f <- (-2:4); f <- f*16^f
# Default ("g") format:
formatC(pi*f)
# Fixed ("f") format, more than one flag ('width' partly "enlarged"):
cbind(formatC(pi*f, digits = 3, width=9, format = "f", flag = "0+"))

formatC(    c("a", "Abc", "no way"), width = -7)  # <=> flag = "-"
formatC(c((-1:1)/0,c(1,100)*pi), width = 8, digits = 1)

## note that some of the results here depend on the implementation
## of long-double arithmetic, which is platform-specific.
xx <- c(1e-12,-3.98765e-10,1.45645e-69,1e-70,pi*1e37,3.44e4)
##       1        2             3        4      5       6
formatC(xx)
formatC(xx, format = "fg")       # special "fixed" format.
formatC(xx[1:4], format = "f", digits = 75) #>> even longer strings

formatC(c(3.24, 2.3e-6), format = "f", digits = 11)
formatC(c(3.24, 2.3e-6), format = "f", digits = 11, drop0trailing = TRUE)

r <- c("76491283764.97430", "29.12345678901", "-7.1234", "-100.1","1123")
## American:
prettyNum(r, big.mark = ",")
## Some Europeans:
prettyNum(r, big.mark = "'", decimal.mark = ",")

(dd <- sapply(1:10, function(i) paste((9:0)[1:i], collapse = "")))
prettyNum(dd, big.mark = "'")

## examples of 'small.mark'
pN <- stats::pnorm(1:7, lower.tail = FALSE)
cbind(format (pN, small.mark = " ", digits = 15))
cbind(formatC(pN, small.mark = " ", digits = 17, format = "f"))

cbind(ff <- format(1.2345 + 10^(0:5), width = 11, big.mark = "'"))
## all with same width (one more than the specified minimum)

## individual formatting to common width:
fc <- formatC(1.234 + 10^(0:8), format = "fg", width = 11, big.mark = "'")
cbind(fc)
## Powers of two, stored exactly, formatted individually:
pow.2 <- formatC(2^-(1:32), digits = 24, width = 1, format = "fg")
## nicely printed (the last line showing 5^32 exactly):
noquote(cbind(pow.2))

## complex numbers:
r <- 10.0000001; rv <- (r/10)^(1:10)
(zv <- (rv + 1i*rv))
op <- options(digits = 7) ## (system default)
(pnv <- prettyNum(zv))
stopifnot(pnv == "1+1i", pnv == format(zv),
          pnv == prettyNum(zv, drop0trailing = TRUE))
## more digits change the picture:
options(digits = 8)
head(fv <- format(zv), 3)
prettyNum(fv)
prettyNum(fv, drop0trailing = TRUE) # a bit nicer
options(op)

## The  '  flag :
doLC <- FALSE # <= R warns, so change to TRUE manually if you want see the effect
if(doLC)
  oldLC <- Sys.setlocale("LC_NUMERIC", "de_CH.UTF-8")
formatC(1.234 + 10^(0:4), format = "fg", width = 11, flag = "'")
## -->  .....  "      1'001" "     10'001"   on supported platforms
if(doLC) ## revert, typically to  "C"  :
  Sys.setlocale("LC_NUMERIC", oldLC)
}
\keyword{character}
\keyword{print}
