\name{nargs}
\title{The Number of Arguments to a Function}
\usage{nargs()}
\alias{nargs}
\description{
  When used inside a function body, \code{nargs} returns the number of
  arguments supplied to that function, \emph{including} positional
  arguments left blank. 
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{\code{\link{args}}, \code{\link{formals}} and \code{\link{sys.call}}.}
\examples{
tst <- function(a, b = 3, ...) {nargs()}
tst() # 0
tst(clicketyclack) # 1 (even non-existing)
tst(c1, a2, rr3) # 3

foo <- function(x, y, z, w) {
   cat("call was", deparse(match.call()), "\n")
   nargs()
}
foo()    # 0
foo(,,3) # 3
foo(z=3) # 1, even though this is the same call

nargs()# not really meaningful
}
\keyword{programming}
