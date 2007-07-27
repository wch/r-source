% File src/library/base/man/sequence.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Development Team
% Distributed under GPL 2 or later

\name{sequence}
\title{Create A Vector of Sequences}
\usage{
sequence(nvec)
}
\alias{sequence}
\arguments{
\item{nvec}{an integer vector each element of which specifies the upper
bound of a sequence.}
}
\description{
For each element of \code{nvec} the sequence \code{seq(nvec[i])}
is created. These are appended and the result returned.
}
\seealso{
\code{\link{gl}}, \code{\link{seq}}, \code{\link{rep}}.
}
\examples{
sequence(c(3,2))# the concatenated sequences 1:3 and 1:2.
#> [1] 1 2 3 1 2
}
\keyword{manip}
