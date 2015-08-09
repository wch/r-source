% File src/library/base/man/Vectorize.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2011 R Core Team
% Copyright 2002-2010 The R Foundation
% Distributed under GPL 2 or later

\name{Vectorize}
\alias{Vectorize}
\title{Vectorize a Scalar Function}
\description{
  \code{Vectorize} creates a function wrapper that vectorizes the
  action of its argument \code{FUN}.
}
\usage{
Vectorize(FUN, vectorize.args = arg.names, SIMPLIFY = TRUE,
          USE.NAMES = TRUE)
}
\arguments{
  \item{FUN}{function to apply, found via \code{\link{match.fun}}.}
  \item{vectorize.args}{a character vector of arguments which should be
    vectorized.  Defaults to all arguments of \code{FUN}.}
  \item{SIMPLIFY}{logical or character string; attempt to reduce the
    result to a vector, matrix or higher dimensional array; see
    the \code{simplify} argument of \code{\link{sapply}}.}
  \item{USE.NAMES}{logical; use names if the first \dots argument has
    names, or if it is a character vector, use that character vector as
    the names.}
}
\details{
  The arguments named in the \code{vectorize.args} argument to
  \code{Vectorize} are the arguments passed in the \code{...}  list to
  \code{\link{mapply}}.  Only those that are actually passed will be
  vectorized; default values will not.  See the examples.

  \code{Vectorize} cannot be used with primitive functions as they do
  not have a value for \code{\link{formals}}.
}
\value{
  A function with the same arguments as \code{FUN}, wrapping a call to
  \code{\link{mapply}}.
}

\examples{
# We use rep.int as rep is primitive
vrep <- Vectorize(rep.int)
vrep(1:4, 4:1)
vrep(times = 1:4, x = 4:1)

vrep <- Vectorize(rep.int, "times")
vrep(times = 1:4, x = 42)

f <- function(x = 1:3, y) c(x, y)
vf <- Vectorize(f, SIMPLIFY = FALSE)
f(1:3, 1:3)
vf(1:3, 1:3)
vf(y = 1:3) # Only vectorizes y, not x

# Nonlinear regression contour plot, based on nls() example
require(graphics)
SS <- function(Vm, K, resp, conc) {
    pred <- (Vm * conc)/(K + conc)
    sum((resp - pred)^2 / pred)
}
vSS <- Vectorize(SS, c("Vm", "K"))
Treated <- subset(Puromycin, state == "treated")

Vm <- seq(140, 310, length.out = 50)
K <- seq(0, 0.15, length.out = 40)
SSvals <- outer(Vm, K, vSS, Treated$rate, Treated$conc)
contour(Vm, K, SSvals, levels = (1:10)^2, xlab = "Vm", ylab = "K")
}
\keyword{manip}
\keyword{utilities}
