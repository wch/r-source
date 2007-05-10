\name{mapply}
\alias{mapply}
\alias{Vectorize}
\title{Apply a function to multiple list or vector arguments }
\description{
  \code{mapply} is a multivariate version of \code{\link{sapply}}.
  \code{mapply} applies \code{FUN} to the first elements of each \dots
  argument, the second elements, the third elements, and so on.
  Arguments are recycled if necessary.

  \code{Vectorize} returns a new function that acts as if \code{mapply}
  was called.
}
\usage{
mapply(FUN, \dots, MoreArgs = NULL, SIMPLIFY = TRUE,
       USE.NAMES = TRUE)

Vectorize(FUN, vectorize.args = arg.names, SIMPLIFY = TRUE,
          USE.NAMES = TRUE)
}
\arguments{
  \item{FUN}{function to apply, found via \code{\link{match.fun}}.}
  \item{\dots}{arguments to vectorize over (list or vector).}
  \item{MoreArgs}{a list of other arguments to \code{FUN}.}
  \item{SIMPLIFY}{logical; attempt to reduce the result to a vector or matrix?}
  \item{USE.NAMES}{logical; use names if the first \dots argument has
    names, or if it is a character vector, use that character vector as
    the names.}
  \item{vectorize.args}{a character vector of arguments which should be
    vectorized.  Defaults to all arguments to \code{FUN}.}
}
\value{
  \code{mapply} returns a list, vector, or matrix.

  \code{Vectorize} returns a function with the same arguments as \code{FUN},
  but wrapping a call to \code{mapply}.
}
\details{
  The arguments named in the \code{vectorize.args} argument to
  \code{Vectorize} correspond to the arguments passed in the \code{...}
  list to \code{mapply}.  However, only those that are actually passed
  will be vectorized; default values will not.  See the example below.

  \code{Vectorize} cannot be used with primitive functions as they have
  no formal list.
}
\seealso{
  \code{\link{sapply}},
  \code{\link{outer}}
}
\examples{
require(graphics)

mapply(rep, 1:4, 4:1)

mapply(rep, times=1:4, x=4:1)

mapply(rep, times=1:4, MoreArgs=list(x=42))

# Repeat the same using Vectorize: use rep.int as rep is primitive
vrep <- Vectorize(rep.int)
vrep(1:4, 4:1)
vrep(times=1:4, x=4:1)

vrep <- Vectorize(rep.int, "times")
vrep(times=1:4, x=42)

mapply(function(x,y) seq_len(x) + y,
       c(a= 1, b=2, c= 3),  # names from first
       c(A=10, B=0, C=-10))

word <- function(C,k) paste(rep.int(C,k), collapse='')
utils::str(mapply(word, LETTERS[1:6], 6:1, SIMPLIFY = FALSE))

f <- function(x=1:3, y) c(x,y)
vf <- Vectorize(f, SIMPLIFY = FALSE)
f(1:3,1:3)
vf(1:3,1:3)
vf(y=1:3) # Only vectorizes y, not x

# Nonlinear regression contour plot, based on nls() example

SS <- function(Vm, K, resp, conc) {
    pred <- (Vm * conc)/(K + conc)
    sum((resp - pred)^2 / pred)
}
vSS <- Vectorize(SS, c("Vm", "K"))
Treated <- subset(Puromycin, state == "treated")

Vm <- seq(140, 310, len=50)
K <- seq(0, 0.15, len=40)
SSvals <- outer(Vm, K, vSS, Treated$rate, Treated$conc)
contour(Vm, K, SSvals, levels=(1:10)^2, xlab="Vm", ylab="K")
}
\keyword{manip}
\keyword{utilities}
