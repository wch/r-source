\name{duplicated}
\title{Determine Duplicate Elements}
\usage{
duplicated(x)
}
\alias{duplicated}
\value{
This function determines which elements of a vector are duplicates
of elements with smaller subscripts, and returns a
logical vector indicating which elements are duplicates.
}
\seealso{
\code{\link{unique}}.
}
\examples{
x <- c(9:20, 1:5, 3:7, 0:8)
# extract unique elements
x[!duplicated(x)]
all(0:20  == sort(x[!duplicated(x)]))
}
\keyword{logic}
\keyword{manip}
