\name{NotYet}
\title{Not Yet Implemented Functions and Unused Arguments}
\alias{NotYetImplemented}
\alias{.NotYetImplemented}
\usage{
.NotYetImplemented()
.NotYetUsed(arg)

symbols(\dots)
plot.lm(\dots)
}
\argumnents{
\item{arg}{an argument of a function that is not yet used.}
}
%------ PLEASE: put \alias{.} here for  EACH !
\alias{symbols}
\alias{plot.lm}
\description{
  In order to pinpoint missing functionality, the \R core team uses
  these functions for missing \R functions and not yet used arguments of
  existing \R functions.

  You are very welcome to contribute your code\dots\dots
}
\seealso{the contrary, \code{\link{Deprecated}} and
  \code{\link{Defunct}} for outdated code.}
\examples{
symbols 		# to see how the ``NotYetImplemented''
.NotYetImplemented	# reference is made automagically
}
\keyword{documentation}
\keyword{utilities}
