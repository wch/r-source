\name{deparse}
\alias{deparse}
\title{Expression Deparsing}
\description{
  Turn unevaluated expressions into character strings.
}
\usage{deparse(expr, width.cutoff = 60)}
\arguments{
  \item{expr}{any \R expression.}
  \item{width.cutoff}{integer in \eqn{[20, 500]} determining the cutoff
    at which line-breaking is tried.}
}
\details{
  This function turns unevaluated expressions (where ``expression'' is
  taken in a wider sense than the strict concept of a vector of mode
  \code{"expression"} used in \code{\link{expression}}) into character
  strings (a kind of inverse \code{\link{parse}}).

  A typical use of this is to create informative labels for data sets
  and plots.  The example shows a simple use of this facility.  It uses
  the functions \code{deparse} and \code{substitute} to create labels
  for a plot which are character string versions of the actual arguments
  to the function \code{myplot}.
}
\seealso{
  \code{\link{substitute}},
  \code{\link{parse}},
  \code{\link{expression}}.
}
\examples{
deparse(args(lm))
deparse(args(lm), width = 100)
myplot <-
function(x, y)
    plot(x, y, xlab=deparse(substitute(x)),
        ylab=deparse(substitute(y)))
}
\keyword{programming}
\keyword{manip}
\keyword{data}
