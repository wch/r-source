\name{deparse}
\title{Expression Deparsing}
\usage{deparse(expr, width.cutoff = 60)}
\alias{deparse}
\arguments{
\item{expr}{any \R expression in a wider sense than \code{\link{expression}}'s
proper.}
\item{width.cutoff}{integer in [20,500] determining the cutoff at which
linebreaking is tried.}
}
\value{
This function turns unevaluated expressions into
character strings (a kind of inverse \code{\link{parse}}).
A typical use of this is to create informative
labels for data sets and plots.  The following example shows a
simple use of this facility.  It uses the functions \code{deparse}
and \code{substitute} to create labels for a plot which are character
string versions of the actual arguments to the function \code{myplot}.
}
\seealso{
\code{\link{substitute}}, \code{\link{parse}}, \code{\link{expression}}.
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
