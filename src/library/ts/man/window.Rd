\name{window}
\title{Time Windows}
\usage{
window(x, start=NULL, end=NULL, warn=TRUE)
}
\alias{window}
\alias{window.default}
\alias{window.ts}
\arguments{
\item{x}{a time-series or other object.}
\item{start}{the start time of the period of interest.}
\item{end}{the end time of the period of interest.}
\item{warn}{if \code{FALSE} omit warnings about \code{start} and
    \code{end} being out of range.}
}
\description{
\code{window} is a generic function which
extracts the subset of the object \code{x}
observed between the times \code{start} and \code{end}.
Methods are available for time series and point-processes.
}
\note{The \code{warn} argument is for use by lazy \R programmers
    whose code requires \code{window} to do the checking silently.}
\seealso{\code{\link{time}}, \code{\link{ts}}.}
\examples{
data(presidents)
sixties <- window(presidents, 1960, c(1969,4))
}
\keyword{ts}
