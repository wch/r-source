\name{substitute}
\title{Actual Arguments}
\usage{
substitute(arg, env=NULL)
quote(arg, env=NULL)
}
\alias{substitute}
\alias{quote}
\description{
\code{substitute} returns the expression which was typed
as the value of a formal argument.  \code{quote} is a synonym useful to lisp
programmers.

The typical use of this is to create informative
labels for data sets and plots.
The following example shows a simple use of this facility.
It uses the functions \code{deparse} and \code{substitute}
to create labels for a plot which are character string versions
of the actual arguments to the function \code{myplot}.
}
\examples{
myplot <- function(x, y)
        plot(x, y, xlab=deparse(substitute(x)),
                ylab=deparse(substitute(y)))
}
\keyword{programming}
\keyword{data}
