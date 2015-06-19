% File src/library/base/man/gl.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{gl}
\alias{gl}
\title{Generate Factor Levels}
\description{
  Generate factors by specifying the pattern of their levels.
}
\usage{
gl(n, k, length = n*k, labels = seq_len(n), ordered = FALSE)
}
\arguments{
  \item{n}{an integer giving the number of levels.}
  \item{k}{an integer giving the number of replications.}
  \item{length}{an integer giving the length of the result.}
  \item{labels}{an optional vector of labels for the resulting factor
    levels.}
  \item{ordered}{a logical indicating whether the result should be
    ordered or not.}
}
\value{
  The result has levels from \code{1} to \code{n} with each value
  replicated in groups of length \code{k} out to a total length of
  \code{length}.

  \code{gl} is modelled on the \emph{GLIM} function of the same name.
}
\seealso{
  The underlying \code{\link{factor}()}.
}
\examples{
## First control, then treatment:
gl(2, 8, labels = c("Control", "Treat"))
## 20 alternating 1s and 2s
gl(2, 1, 20)
## alternating pairs of 1s and 2s
gl(2, 2, 20)
}
\keyword{category}
\keyword{arith}
