\name{cbind}
\title{Combine Columns into a Matrix}
\usage{
cbind(\dots)
}
\alias{cbind}
\value{
\code{cbind} takes a sequence of vector and/or matrix arguments
and combines them as the columns of a matrix.

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
\code{\link{c}},
\code{\link{rbind}}.
}
\examples{
cbind(1,1:n)
}
\keyword{array}
\keyword{manip}
