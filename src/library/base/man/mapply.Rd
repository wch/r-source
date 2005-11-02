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
mapply(FUN, \dots, MoreArgs = NULL, SIMPLIFY = TRUE, USE.NAMES = TRUE)
Vectorize(FUN, vectorize.args = arg.names, SIMPLIFY = TRUE, USE.NAMES = TRUE)
}
\arguments{
  \item{FUN}{Function to apply }
  \item{\dots}{Arguments to vectorise over (list or vector) }
  \item{MoreArgs}{A list of other arguments to \code{FUN} }
  \item{SIMPLIFY}{Attempt to reduce the result to a vector or matrix? }
  \item{USE.NAMES}{If the first \dots argument is character and the
    result doesn't already have names, use it as the names}
  \item{vectorize.args}{A character vector of arguments which should be vectorized.  Defaults
  to all arguments to \code{FUN}}
}
\value{
  \code{mapply} returns a list, vector, or matrix.
  
  \code{Vectorize} returns a function with the same arguments as \code{FUN},
  but wrapping a call to \code{mapply}.
}
\details{
  The arguments named in the \code{vectorize.args} argument to \code{Vectorize}
  correspond to the arguments passed in the \code{...} list to \code{mapply}.  However,
  only those that are actually passed will be vectorized; default values will not.
  See the example below.
}
\seealso{
  \code{\link{sapply}},
  \code{\link{outer}}
}
\examples{
mapply(rep, 1:4, 4:1)

mapply(rep, times=1:4, x=4:1)

mapply(rep, times=1:4, MoreArgs=list(x=42))

# Repeat the same using Vectorize
vrep <- Vectorize(rep)
vrep(1:4, 4:1)
vrep(times=1:4, x=4:1)

vrep <- Vectorize(rep, "times")
vrep(times=1:4, x=42)

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
