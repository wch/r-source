\name{window}
\title{Time Windows}
\usage{
window(x, start, end)
}
\alias{window}
\arguments{
\item{x}{a time-series or other object.}
\item{start}{the start time of the period of interest.}
\item{end}{the end time of the period of interest.}
}
\description{
\code{window} is a generic function which
extracts the subset of the object \code{x}
observed between the times \code{start} and \code{end}.
Methods are available for time series and point-processes.
}
\seealso{
\code{\link{time}},
\code{\link{ts}}.
}
\examples{
data(presidents)
sixties <- window(presidents, 1960, c(1969,4))
}
\keyword{ts}
