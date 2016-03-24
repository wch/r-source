% File src/library/utils/man/roman.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2016 R Core Team
% Distributed under GPL 2 or later

\name{roman}
\title{Roman Numerals}
\alias{as.roman}
\alias{.romans}
\description{
  Simple manipulation of (a small set of) integer numbers as roman numerals.
}
\usage{
as.roman(x)
.romans
}
\arguments{
  \item{x}{a numeric or character vector of arabic or roman numerals.}
}
\details{
  \code{as.roman} creates objects of class \code{"roman"} which are
  internally represented as integers, and have suitable methods for
  printing, formatting, subsetting, coercion, etc, see
  \code{\link{methods}(class = "roman")}.

  Only numbers between 1 and 3899 have a unique representation as roman
  numbers.

  \code{.romans} is the basic dictionnary, a named \code{\link{character}} vector.
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

## mixture of arabic and roman numbers :
as.roman(c(NA, 1:3, "", strrep("I", 1:6))) # + NA with a warning for "IIIIII"
cc <- c(NA, 1:3, strrep("I", 0:5))
(rc <- as.roman(cc)) # two NAs: 0 is not "roman"
(ic <- as.integer(rc)) # works automitcally [without an explicit method]
## simple consistency checks -- arithmetic when result is in  {1,2,..,3899} :
stopifnot(identical(rc, as.roman(rc)), # as.roman(.) is "idempotent"
          identical(rc + rc + (3*rc), rc*5),
          identical(ic, c(NA, 1:3, NA, 1:5)),
          identical(as.integer(5*rc), 5L*ic),
          identical(as.numeric(rc), as.numeric(ic)),
          identical(as.list(rc), as.list(ic)))
}
\keyword{arith}
