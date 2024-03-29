% File src/library/datasets/man/randu.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2024 R Core Team
% Distributed under GPL 2 or later

\name{randu}
\docType{data}
\alias{randu}
\title{Random Numbers from \I{Congruential} Generator \I{RANDU}}
\description{
  400 triples of successive random numbers were taken from the VAX
  FORTRAN function \I{RANDU} running under \I{VMS} 1.5.
}
\usage{randu}
\details{
  In three dimensional displays it is evident that the triples fall on
  15 parallel planes in 3-space. This can be shown theoretically to be
  true for all triples from the \I{RANDU} generator.

  These particular 400 triples start 5 apart in the sequence, that is
  they are ((U[5i+1], U[5i+2], U[5i+3]), i= 0, \dots, 399), and they
  are rounded to 6 decimal places.

  Under \I{VMS} versions 2.0 and higher, this problem has been fixed.
}
\format{
  A data frame with 400 observations on 3 variables named \code{x},
  \code{y} and \code{z} which give the first, second and third random
  number in the triple.
}
\source{
  David Donoho
}
\examples{
## We could re-generate the dataset by the following R code
seed <- as.double(1)
RANDU <- function() {
    seed <<- ((2^16 + 3) * seed) \%\% (2^31)
    seed/(2^31)
}
myrandu <- matrix(NA_real_, 400, 3, dimnames = list(NULL, c("x","y","z")))
for(i in 1:400) {
    U <- c(RANDU(), RANDU(), RANDU(), RANDU(), RANDU())
    myrandu[i,] <- round(U[1:3], 6)
}
stopifnot(all.equal(randu, as.data.frame(myrandu), tolerance = 1e-5))
}
\keyword{datasets}
