\name{NotYet}
\title{Not Yet Implemented Functions and Unused Arguments}
\alias{NotYetImplemented}
\alias{.NotYetImplemented}
\alias{NotYetUsed}
\alias{.NotYetUsed}
\usage{
.NotYetImplemented()
.NotYetUsed(arg)

symbols(\dots)
}
\arguments{
  \item{arg}{an argument of a function that is not yet used.}
}
%------ PLEASE: put \alias{.} here for  EACH !
\alias{symbols}
%done\alias{plot.lm}
\description{
  In order to pinpoint missing functionality, the \R core team uses
  these functions for missing \R functions and not yet used arguments of
  existing \R functions (which are typically there for compatibility
  purposes).

  You are very welcome to contribute your code \dots
}
\seealso{the contrary, \code{\link{Deprecated}} and
  \code{\link{Defunct}} for outdated code.}
\examples{
symbols                     # to see how the ``NotYetImplemented''
.NotYetImplemented          # reference is made automagically

barplot(1:5, inside = TRUE) # `inside' is not yet used
}
\keyword{documentation}
\keyword{utilities}
