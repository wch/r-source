\name{NotYet}
\title{Not Yet Implemented Functions and Unused Arguments}
\alias{NotYetImplemented}
\alias{.NotYetImplemented}
\alias{NotYetUsed}
\alias{.NotYetUsed}
\usage{
.NotYetImplemented()
.NotYetUsed(arg)
}
\arguments{
  \item{arg}{an argument of a function that is not yet used.}
}
%------ PLEASE: put \alias{.} here for  EACH !
%done\alias{symbols}
%done\alias{plot.lm}
%NOT done, but in ./plot.lm.Rd on purpose:  \alias{plot.mlm}
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
plot.mlm         # to see how the ``NotYetImplemented''
                 # reference is made automagically
\testonly{options(error = expression(NULL))# allowing "make check"}
plot.mlm()
\testonly{options(error = NULL) # back to Default}

barplot(1:5, inside = TRUE) # `inside' is not yet used
}
\keyword{documentation}
\keyword{utilities}
