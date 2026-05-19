% File src/library/base/man/zapsmall.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2026 R Core Team
% Distributed under GPL 2 or later

\name{zapsmall}
\alias{zapsmall}
\title{Rounding of Numbers: Zapping Small Ones to Zero}
\usage{
zapsmall(x, digits = getOption("digits"),
         mFUN = function(x, ina) max(abs(x[!ina])),
         min.d = 0L)
}
\description{
  \code{zapsmall} determines a \code{digits} argument \code{dr} for
  calling \code{round(x, digits = dr)} such that values close to
  zero (compared with the maximal absolute value in the vector) are
  \sQuote{zapped}, i.e., replaced by \code{0}.
  \code{digits = Inf} keeps the full precision, returning \code{x} unchanged.
}
\arguments{
  \item{x}{a numeric or complex vector or any \R number-like object
    which has a \code{\link{round}} method and basic arithmetic methods
    including \code{\link{log10}()}.}
  \item{digits}{integer indicating the precision to be used.}
  \item{mFUN}{a \code{\link{function}(x, ina)} of the numeric (or complex)
    \code{x} and  the \code{\link{logical}} \code{ina := is.na(x)}
    returning a positive number in the order of magnitude of the maximal
    \code{abs(x)} value.  The default is back compatible but not robust,
    and e.g., not very useful when \code{x} has infinite entries.}
  \item{min.d}{an integer specifying the minimal number of digits to use in
    the resulting \code{\link{round}(x, digits=*)} call when \code{mFUN(*) > 0}.
    Using \code{min.d = -324} or smaller provides scale invariance,
    whereas the default \code{min.d = 0} is back compatible with S and
    \R versions before 4.4.0 where, it was implicitly hardwired to \code{0}
    with the intuition that e.g., \code{4} should never be zapped to \code{0}.}
}
\references{
  \bibshow{R:Chambers:1998}
}
\examples{
x2 <- pi * 100^(-2:2)/10
   print(  x2, digits = 4)
zapsmall(  x2) # automatic digits
zapsmall(  x2,   digits = 4) # 0 0 0.3 ...
zapsmall(1e6*x2, digits = 4) #  no zeros (*not* scale invariant)
zapsmall(1e6*x2, digits = 4, min.d = -324) # 0 0 .. *is* scale inv.
zapsmall(c(x2, Inf)) # round()s to integer, as min.d = 0
zapsmall(c(x2, Inf), min.d=-Inf) # 0 0 .. 0 Inf -- everything  is small wrt  Inf

(z <- exp(1i*0:4*pi/2))
zapsmall(z)

zapShow <- function(x, ...) rbind(orig = x, zapped = zapsmall(x, ...))
zapShow(x2)

## using a *robust* mFUN
mF_rob <- function(x, ina) boxplot.stats(x, do.conf=FALSE)$stats[5]
## a simpler robust one:
mF_Q3  <- function(x, ina) { x <- abs(x[is.finite(x)])
    if(length(x <- x[x > 0])) quantile(x, 3/4, names=FALSE) else 0 }
## at least "protect from Inf":
mF_mxF <- function(x, ina) max(abs(x[is.finite(x)]))
## with robust mFUN(), 'Inf' is no longer distorting the picture:
zapShow(c(x2, Inf), mFUN = mF_Q3) # no zapping
zapShow(c(x2, Inf), mFUN = mF_Q3, min.d = -5) # the same
zapShow(c(x2, Inf), mFUN = mF_Q3, min.d = -324) #   same
zapShow(c(x2, 9999), mFUN = mF_Q3) # zap 1st
zapShow(c(x2, 9999), mFUN = mF_Q3, min.d = 3) # the same
zapShow(c(x2, 9999), mFUN = mF_Q3, min.d = -324)# ditto
zapShow(c(x2, 9999), mFUN = mF_Q3, min.d = 8) #  no zap

zapShow(c(x2, Inf), mFUN = mF_mxF) # no zapping
zapShow(c(x2, Inf), mFUN = mF_mxF, min.d = -5) # the same
zapShow(c(x2, Inf), mFUN = mF_mxF, min.d = -324) #   same
zapShow(c(x2, 9999), mFUN = mF_mxF)
zapShow(c(x2, 9999), mFUN = mF_mxF, min.d = 3) # the same
zapShow(c(x2, 9999), mFUN = mF_mxF, min.d = -324)# ditto
zapShow(c(x2, 9999), mFUN = mF_mxF, min.d = 8) # small diff

zapShow(c(x2, Inf), mFUN = mF_rob)
zapShow(c(x2, Inf), mFUN = mF_rob, min.d = -5) # the same
zapShow(c(x2, 9999), mFUN = mF_rob) # same *rounding* as w/ Inf
zapShow(c(x2, 9999), mFUN = mF_rob, min.d = 3) # the same
zapShow(c(x2, 9999), mFUN = mF_rob, min.d = 8) # small diff
}
\keyword{arith}
