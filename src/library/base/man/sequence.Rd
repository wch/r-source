% File src/library/base/man/sequence.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{sequence}
\title{Create A Vector of Sequences}
\usage{
sequence(nvec)
}
\alias{sequence}
\arguments{
  \item{nvec}{a non-negative integer vector each element of which
    specifies the end point of a sequence.}
}
\description{
  For each element of \code{nvec} the sequence \code{\link{seq_len}(nvec[i])}
  is created.  These are concatenated and the result returned.
}
\details{
  Earlier versions of \code{sequence} used to work for 0 or negative inputs as
  \code{seq(x) == 1:x}.

  Note that \code{sequence <- function(nvec) unlist(lapply(nvec, seq_len))}
  and it mainly exists in reverence to the very early history of \R.
}
\seealso{
  \code{\link{gl}}, \code{\link{seq}}, \code{\link{rep}}.
}
\examples{
sequence(c(3, 2)) # the concatenated sequences 1:3 and 1:2.
#> [1] 1 2 3 1 2
}
\keyword{manip}
