\name{NotYet}
\title{Not Yet Implemented Functions and Unused Arguments}
\alias{NotYetImplemented}
\alias{.NotYetImplemented}
\alias{NotYetUsed}
\alias{.NotYetUsed}
\description{
  In order to pinpoint missing functionality, the \R core team uses
  these functions for missing \R functions and not yet used arguments of
  existing \R functions (which are typically there for compatibility
  purposes).

  You are very welcome to contribute your code \dots
}
\usage{
.NotYetImplemented()
.NotYetUsed(arg, error = TRUE)
}
\arguments{
  \item{arg}{an argument of a function that is not yet used.}
  \item{error}{a logical.  If \code{TRUE}, an error is signalled; if
    \code{FALSE}; only a warning is given.}
}
%------ PLEASE: put \alias{.} here for  EACH !
%done\alias{symbols}
%done\alias{plot.lm}
%NOT done, but in ./plot.lm.Rd on purpose:  \alias{plot.mlm}
\seealso{the contrary, \code{\link{Deprecated}} and
  \code{\link{Defunct}} for outdated code.}
\examples{
require(graphics)
require(stats)
plot.mlm         # to see how the "NotYetImplemented"
                 # reference is made automagically
try(plot.mlm())

barplot(1:5, inside = TRUE) # 'inside' is not yet used
}
\keyword{documentation}
\keyword{utilities}
