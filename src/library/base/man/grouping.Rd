% File src/library/base/man/grouping.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2016 R Core Team
% Distributed under GPL 2 or later

\name{grouping}
\title{Grouping Permutation}
\alias{grouping}
\concept{aggregation}
\description{
  \code{grouping} returns a permutation which rearranges its first
  argument such that identical values are adjacent to each other.  Also
  returned as attributes are the group-wise partitioning and the maximum
  group size.
}
\usage{
grouping(\dots)
}
\arguments{
  \item{\dots}{a sequence of numeric, character or logical
    vectors, all of the same length, or a classed \R object.}
}
\details{
  The function partially sorts the elements so that identical values are
  adjacent. \code{NA} values come first.  This is guaranteed to be
  stable, so ties are preserved, and if the data are already
  grouped/sorted, the grouping is unchanged.  This is useful for
  aggregation and is particularly fast for character vectors.

  Under the covers, the \code{"radix"} method of \code{\link{order}} is
  used, and the same caveats apply, including reduced precision when
  comparing real values, restrictions on character encodings and lack of
  support for long vectors (those with eqn{2^{31}}{2^31} or more elements).
  
  Like \code{order}, for a classed \R object the grouping is based on
  the result of \code{\link{xtfrm}}.
}

\value{
  An object of class \code{"grouping"}, the representation of which
  should be considered experimental and subject to change.  It is an
  integer vector with two attributes:
  \item{ends}{subscripts in the result corresponding to the last
    member of each group}
  \item{maxgrpn}{the maximum group size}
}

\seealso{
  \code{\link{order}}, \code{\link{xtfrm}}.
}
\examples{
(ii <- grouping(x <- c(1, 1, 3:1, 1:4, 3), y <- c(9, 9:1), z <- c(2, 1:9)))
## 6  5  2  1  7  4 10  8  3  9
rbind(x, y, z)[, ii]
}
\keyword{manip}
