\name{data.matrix}
\title{Data Frame to Numeric Matrix}
\usage{
data.matrix(frame)
}
\alias{data.matrix}
\arguments{
\item{frame}{a data frame whose components are either
logical vectors, factors or numeric vectors.}
}
\value{
The matrix obtained by converting all the variables in a
data frame to numeric mode and then binding them together
as the columns of a matrix.
Factors and ordered factors are replaced by their codes.
}
\seealso{
\code{\link{as.matrix}},
\code{\link{codes}},
\code{\link{data.frame}},
\code{\link{matrix}}.
}
\keyword{array}
