% File src/library/utils/man/roman.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2025 R Core Team
% Distributed under GPL 2 or later

\name{roman}
\title{Roman Numerals}
\alias{as.roman}
\alias{.romans}
\alias{Ops.roman}
\alias{Summary.roman}
\description{
  Simple manipulation of (a small set of) integer numbers as roman numerals.
}
\usage{
as.roman(x)
.romans
\special{% no \alias
r1 + r2
r1 <= r2
max(r1)
sum(r2)
}
}
\arguments{
  \item{x}{a numeric or character vector of arabic or roman numerals.}
  \item{r1, r2}{a roman number vector, i.e., of \code{\link{class}}
    \code{"roman"}.}
}
\details{
  \code{as.roman} creates objects of class \code{"roman"} which are
  internally represented as integers, and have suitable methods for
  printing, formatting, subsetting, coercion, etc, see
  \code{\link{methods}(class = "roman")}.

  Arithmetic (\code{"\link{Arith}"}), Comparison (\code{"\link{Compare}"})
  and  (\code{"\link{Logic}"}), i.e., all \code{"\link{Ops}"} group
  operations work as for regular numbers via \R's integer functionality.

  Only numbers between 1 and 4999 are \dQuote{well} representable as roman
  numbers, and hence others result in \code{as.roman(NA)}.

  \code{.romans} is the basic dictionary, a named \code{\link{character}} vector.
}
\references{
  Wikipedia contributors (2024). Roman numerals.
  Wikipedia, The Free Encyclopedia.
  \url{https://en.wikipedia.org/w/index.php?title=Roman_numerals&oldid=1188781837}.
  Accessed February 22, 2024.
}
\examples{
## First five roman 'numbers'.
(y <- as.roman(1 : 5))
## Middle one.
y[3]
## Current year as a roman number.
(y <- as.roman(format(Sys.Date(), "\%Y")))
## Today, and  10, 20, 30, and 100 years ago ...
y - 10*c(0:3,10)
\dontshow{stopifnot(identical(as.character(as.roman("2016") - 10*c(0:3,10)),
             c("MMXVI", "MMVI", "MCMXCVI", "MCMLXXXVI", "MCMXVI"))) }
## mixture of arabic and roman numbers :
as.roman(c(NA, 1:3, "", strrep("I", 1:7))) # + NA with a warning for the last.
cc <- c(NA, 1:3, strrep("I", 0:6)) # "IIIIII" is historical (Wikipedia)
(rc <- as.roman(cc)) # two NAs: 0 is not "roman"
(ic <- as.integer(rc)) # works automatically [without an explicit method]
rNA <- as.roman(NA)
## simple consistency checks -- arithmetic when result is in  {1,2,..,4999} :
stopifnot(identical(rc, as.roman(rc)), # as.roman(.) is "idempotent"
          identical(rc + rc + (3*rc), rc*5),
          identical(ic, c(NA, 1:3, NA, 1:6)),
          identical(as.integer(5*rc), 5L*ic),
          identical(as.numeric(rc), as.numeric(ic)),
          identical(rc[1], rNA),
          identical(as.roman(0), rNA),
          identical(as.roman(NA_character_), rNA),
          identical(as.list(rc), as.list(ic)))
## Non-Arithmetic 'Ops' :
stopifnot(exprs = {
        # Comparisons :
        identical(ic <= (ii <- c(0:4,1:6)), rc <= ii)
        identical(ic == ii, rc == as.roman(ii))
        # Logic  [integers |-> logical] :
        identical(rc & TRUE , ic & TRUE)
        identical(rc & FALSE, ic & FALSE)
        identical(rc | FALSE, ic | FALSE)
        identical(rc | NA   , ic | NA)
})
## 'Summary' group functions (and comparison):
(rc. <- rc[!is.na(rc)])
stopifnot(exprs = {
        identical(min(rc), as.roman(NA))
        identical(min(rc, na.rm=TRUE),
         as.roman(min(ic, na.rm=TRUE)))
        identical(range(rc.),
         as.roman(range(as.integer(rc.))))
        identical(sum (rc, na.rm=TRUE), as.roman("XXVII"))
        identical(format(prod(rc, na.rm=TRUE)), "MMMMCCCXX")
                  format(prod(rc.)) ==          "MMMMCCCXX"
        identical(format(prod(rc[-11], na.rm=TRUE)), "DCCXX")
})
}
\keyword{arith}
