\name{Extract}
\title{Extract or Replace Parts of an Object}
\usage{
x[i]
x[i, j, \dots]
x[i, j, \dots , drop=TRUE]
x[[i]]
x[[i, j, \dots]]
x$name
}
\alias{Extract}
\alias{Subscript}
\alias{[}
\alias{[[}
\alias{$}
\alias{[<-}
\alias{[[<-}
\alias{$<-}
%- methods
\alias{[.data.frame}
\alias{[[.data.frame}
\alias{[<-.data.frame}
\alias{[[<-.data.frame}
\alias{[.factor}
\alias{[.formula}
\alias{[.ts}
\alias{[<-.factor}
\alias{[.noquote}
\alias{[<-.noquote}
\description{
These operators act on vectors, arrays, dataframes and lists.
If one of these expressions appears on the left side of an assignment
then that part of \code{x} is set to the value of the right hand side
of the assignment.

These operators are generic. You can write methods to handle subsetting
of specific classes of data.

The \code{[[} operator requires all relevant subscripts be supplied.
With  the \code{[} operator a comma separated blank indicates that all
entries in that dimension are selected.

When operating on a list, the \code{[[} operator gives the specified
element of the list while the \code{[} operator returns a list with
the specified element(s) in it.
}
\seealso{
\code{\link{list}}, \code{\link{array}}, \code{\link{matrix}}.
}
\examples{
x <- 1:12; m <- matrix(1:6,nr=2); li <- list(pi=pi, e = exp(1))
x[10]		      # the tenth element of x
m[1,]		      # the first row of matrix m
m[1, , drop = FALSE]  # is a 1-row matrix
li[[1]]		      # the first element of list li
y <- list(1,2,a=4,5)
y[c(3,4)]	      # a list containing elements 3 and 4 of y
y$a		      # the element of y named a
}
\keyword{array}
\keyword{list}
