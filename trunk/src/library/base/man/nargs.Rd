% File src/library/base/man/nargs.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{nargs}
\title{The Number of Arguments to a Function}
\usage{nargs()}
\alias{nargs}
\description{
  When used inside a function body, \code{nargs} returns the number of
  arguments supplied to that function, \emph{including} positional
  arguments left blank.
}
\details{
  The count includes empty (missing) arguments, so that \code{foo(x,,z)}
  will be considered to have three arguments (see \sQuote{Examples}).
  This can occur in rather indirect ways, so for example \code{x[]}
  might dispatch a call to \code{`[.some_method`(x, )} which is
  considered to have two arguments.

  This is a \link{primitive} function.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{args}}, \code{\link{formals}} and \code{\link{sys.call}}.
}
\examples{
tst <- function(a, b = 3, ...) {nargs()}
tst() # 0
tst(clicketyclack) # 1 (even non-existing)
tst(c1, a2, rr3) # 3

foo <- function(x, y, z, w) {
   cat("call was ", deparse(match.call()), "\n", sep = "")
   nargs()
}
foo()      # 0
foo(, , 3) # 3
foo(z = 3) # 1, even though this is the same call

nargs()  # not really meaningful
}
\keyword{programming}
