\name{start}
\title{Encode the Terminal Times of Time Series}
\usage{
start(x, \dots)
end(x, \dots)
}
\alias{start}
\alias{end}
%\alias{start.default}
%\alias{end.default}
\arguments{
    \item{x}{a univariate or multivariate time-series, or a vector or matrix.}
    \item{\dots}{extra arguments for future methods.}
}
\description{
    Extract and encode the times the first and last observations were
    taken. Provided only for compatibility with S version 2.
}
\details{
    These are generic functions, which will use the
    \code{\link{tsp}} attribute of \code{x} if it exists.
    Their default methods decode the start time from the original time
    units, so that for a monthly series \code{1995.5} is represented
    as \code{c(1995, 7)}. For a series of frequency \code{f}, time
    \code{n+i/f} is presented as \code{c(n, i+1)} (even for \code{i = 0}
    and \code{f = 1}).
}
\section{Warning}{
    The representation used by \code{start} and \code{end} has no
    meaning unless the frequency is supplied.
}
\seealso{
  \code{\link{ts}}, \code{\link{time}}, \code{\link{tsp}}.
}
\keyword{ts}
