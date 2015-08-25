% File src/library/base/man/sweep.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2010 R Core Team
% Distributed under GPL 2 or later

\name{sweep}
\alias{sweep}
\title{Sweep out Array Summaries}
\description{
  Return an array obtained from an input array by sweeping out a summary
  statistic.
}
\usage{
sweep(x, MARGIN, STATS, FUN = "-", check.margin = TRUE, \dots)
}
\arguments{
  \item{x}{an array.}
  \item{MARGIN}{a vector of indices giving the extent(s) of \code{x}
    which correspond to \code{STATS}.}
  \item{STATS}{the summary statistic which is to be swept out.}
  \item{FUN}{the function to be used to carry out the sweep.}
  \item{check.margin}{logical.  If \code{TRUE} (the default), warn if the
    length or dimensions of \code{STATS} do not match the specified
    dimensions of \code{x}.  Set to \code{FALSE} for a small speed gain
    when you \emph{know} that dimensions match.}
  \item{\dots}{optional arguments to \code{FUN}.}
}
\details{
  \code{FUN} is found by a call to \code{\link{match.fun}}.  As in the
  default, binary operators can be supplied if quoted or backquoted.

  \code{FUN} should be a function of two arguments: it will be called
  with arguments \code{x} and an array of the same dimensions generated
  from \code{STATS} by \code{\link{aperm}}.

  The consistency check among \code{STATS}, \code{MARGIN} and \code{x}
  is stricter if \code{STATS} is an array than if it is a vector.
  In the vector case, some kinds of recycling are allowed without a
  warning.  Use \code{sweep(x, MARGIN, as.array(STATS))} if \code{STATS}
  is a vector and you want to be warned if any recycling occurs.
}
\value{
  An array with the same shape as \code{x}, but with the summary
  statistics swept out.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{apply}} on which \code{sweep} used to be based;
  \code{\link{scale}} for centering and scaling.
}
\examples{
require(stats) # for median
med.att <- apply(attitude, 2, median)
sweep(data.matrix(attitude), 2, med.att)  # subtract the column medians

## More sweeping:
A <- array(1:24, dim = 4:2)

## no warnings in normal use
sweep(A, 1, 5)
(A.min <- apply(A, 1, min))  # == 1:4
sweep(A, 1, A.min)
sweep(A, 1:2, apply(A, 1:2, median))

## warnings when mismatch
sweep(A, 1, 1:3)  # STATS does not recycle
sweep(A, 1, 6:1)  # STATS is longer

## exact recycling:
sweep(A, 1, 1:2)  # no warning
sweep(A, 1, as.array(1:2))  # warning
}
\keyword{array}
\keyword{iteration}
