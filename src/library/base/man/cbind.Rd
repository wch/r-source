\name{cbind}
\title{Combine Columns into a Matrix}
\usage{
cbind(\dots)
}
\alias{cbind}
\value{
\code{cbind} takes a sequence of vector and/or matrix arguments
and combines them as the \bold{c}olumns of a matrix.

If there are several matrix arguments,
they must all have the same number of rows
and this will be the number of rows of the result.
If all the arguments are vectors, the number of rows in the
result is equal to the length of the longest vector.
Values in shorter arguments are recycled
to achieve this length.

When the arguments consist of a mix of matrices and vectors
the number of rows of the result is determined by the number
of rows of the matrix arguments.  Any vectors have their
values recycled or subsetted to achieve this length.
}
\seealso{
\code{\link{c}} to combine vectors or list;
\code{\link{rbind}} to combine \bold{r}ows.
}
\examples{
cbind(1,1:7) # the '1' (= shorter vector) is recycled
cbind(1:7, diag(3))# vector is subset

cbind(0, matrix(1, nrow=0, ncol=4))#> Warning (making sense)
dim(cbind(0, matrix(1, nrow=2, ncol=0)))#-> 2 x 1
}
\keyword{array}
\keyword{manip}
