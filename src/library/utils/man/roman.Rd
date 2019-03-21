% File src/library/utils/man/roman.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2019 R Core Team
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

  Only numbers between 1 and 3899 have a unique representation as roman
  numbers, and hence others result in \code{as.roman(NA)}.

  \code{.romans} is the basic dictionary, a named \code{\link{character}} vector.
}
\references{
  Wikipedia contributors (2006). Roman numerals.
  Wikipedia, The Free Encyclopedia.
  \url{https://en.wikipedia.org/w/index.php?title=Roman_numerals&oldid=78252134}.
  Accessed September 29, 2006.
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
as.roman(c(NA, 1:3, "", strrep("I", 1:6))) # + NA with a warning for "IIIIII"
cc <- c(NA, 1:3, strrep("I", 0:5))
(rc <- as.roman(cc)) # two NAs: 0 is not "roman"
(ic <- as.integer(rc)) # works automatically [without an explicit method]
rNA <- as.roman(NA)
## simple consistency checks -- arithmetic when result is in  {1,2,..,3899} :
stopifnot(identical(rc, as.roman(rc)), # as.roman(.) is "idempotent"
          identical(rc + rc + (3*rc), rc*5),
          identical(ic, c(NA, 1:3, NA, 1:5)),
          identical(as.integer(5*rc), 5L*ic),
          identical(as.numeric(rc), as.numeric(ic)),
          identical(rc[1], rNA),
          identical(as.roman(0), rNA),
          identical(as.roman(NA_character_), rNA),
          identical(as.list(rc), as.list(ic)))
## Non-Arithmetic 'Ops' :
stopifnot(exprs = {
        # Comparisons :
        identical(ic < 1:5, rc < 1:5)
        identical(ic < 1:5, rc < as.roman(1:5))
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
        identical(sum (rc, na.rm=TRUE), as.roman("XXI"))
        identical(format(prod(rc, na.rm=TRUE)), "DCCXX")
        format(prod(rc.)) == "DCCXX"
})
}
\keyword{arith}
