\name{sweep}
\title{Sweep out Array Summaries}
\usage{
sweep(x, MARGIN, STATS, FUN="-", \dots)
}
\alias{sweep}
\arguments{
\item{x}{an array.}
\item{MARGIN}{a giving the extents of \code{x} which correspond to \code{STATS}.}
\item{STATS}{the summary statistic which is to be swept out.}
\item{FUN}{the function to be used to carry out the sweep.
In the case of binary operators such as \code{"/"} etc.,
the function name must be quoted.}
\item{\dots}{optional arguments to \code{FUN}.}
}
\value{
An array with the same shape as \code{x}, but with the summary
statistics swept out.
}
\seealso{
\code{\link{apply}}.
}
\examples{
data(attitude)
med.att <- apply(attitude, 2, median)
sweep(data.matrix(attitude), 2, med.att)# subtract the column medians
}
\keyword{array}
\keyword{iteration}
