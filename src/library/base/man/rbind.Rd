\name{rbind}
\title{Combine Rows into a Matrix}
\usage{rbind(\dots)}
\alias{rbind}
\value{
\code{rbind} takes a sequence of vector and/or matrix arguments
and combines them as the rows of a matrix.

If there are several matrix arguments,
they must all have the same number of columns
and this will be the number of columns of the result.
If all the arguments are vectors, the number of columns in the
result is equal to the length of the longest vector.
Values in shorter arguments are recycled
to achieve this length.

When the arguments consist of a mix of matrices and vectors
the number of columns of the result is determined by the number
of columns of the matrix arguments.  Any vectors have their
values recycled or subsetted to achieve this length.
}
\seealso{
\code{\link{c}},
\code{\link{cbind}}.
}
\examples{
rbind(1,1:n)
}
\keyword{array}
\keyword{manip}
