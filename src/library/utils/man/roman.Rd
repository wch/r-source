% File src/library/utils/man/roman.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{roman}
\alias{as.roman}
\title{Roman Numerals}
\description{
  Manipulate integers as roman numerals.
}
\usage{
as.roman(x)
}
\arguments{
  \item{x}{a numeric vector, or a character vector of arabic or roman
    numerals.}
}
\details{
  \code{as.roman} creates objects of class \code{"roman"} which are
  internally represented as integers, and have suitable methods for
  printing, formatting, subsetting, and coercion to \code{character}.

  Only numbers between 1 and 3899 have a unique representation as roman
  numbers.
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
## 10 years ago ...
y - 10
}
\keyword{arith}
