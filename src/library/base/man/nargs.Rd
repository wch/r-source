\name{nargs}
\title{The Number of Arguments to a Function}
\usage{nargs()}
\alias{nargs}
\description{
  When used inside a function body, \code{nargs} returns the number of
  arguments supplied to that function.
}
\seealso{\code{\link{args}}, \code{\link{formals}} and \code{\link{sys.call}}.}
\examples{
tst <- function(a, b = 3, ...) {nargs()}
tst() # 0
tst(clicketyclack) # 1 (even non-existing)
tst(c1, a2, rr3) # 3

nargs()# not really meaningful
}
\keyword{programming}
