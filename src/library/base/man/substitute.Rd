\name{substitute}
\title{Actual Arguments}
\usage{
substitute(expr, env=NULL)
quote(expr, env=NULL)
}
\alias{substitute}
\alias{quote}
\description{
\code{substitute} returns the expression which was typed
as the value of a formal argument.
\code{quote} is a synonym useful to lisp programmers.
}
\details{
The typical use of this is to create informative
labels for data sets and plots.
The \code{myplot} example below shows a simple use of this facility.
It uses the functions \code{\link{deparse}} and \code{substitute}
to create labels for a plot which are character string versions
of the actual arguments to the function \code{myplot}.
}
\seealso{\code{\link{missing}} for argument ``missingness''.}
\examples{
substitute(expression(a + b), list(a = 1))#> expression(1 + b)

myplot <- function(x, y)
        plot(x, y, xlab=deparse(substitute(x)),
                ylab=deparse(substitute(y)))

## Simple examples about lazy evaluation, etc:

f1 <- function(x, y = x)             { x <- x + 1; y }
s1 <- function(x, y = substitute(x)) { x <- x + 1; y }
s2 <- function(x, y) { if(missing(y)) y <- substitute(x); x <- x + 1; y }
a <- 10
f1(a)# 11
s1(a)# 11
s2(a)# a
typeof(s2(a))# "symbol"
}
\keyword{programming}
\keyword{data}
