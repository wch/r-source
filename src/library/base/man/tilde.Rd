% File src/library/base/man/tilde.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2021 R Core Team
% Distributed under GPL 2 or later

\name{tilde}
\alias{~}
\alias{tilde}
\title{Tilde Operator}
\description{
  Tilde is used to separate the left- and right-hand sides in a model formula.
}
\usage{
y ~ model
}
\arguments{
  \item{y, model}{symbolic expressions.}
}
\details{
  The left-hand side is optional, and one-sided formulae are used in
  some contexts.

  A formula has \link{mode} \code{\link{call}}.  It can be subsetted by
  \code{[[}: the components are \code{~}, the left-hand side (if
  present) and the right-hand side \emph{in that order}.  (Thus
  one-sided formulae have two components.)
}
\references{
  \bibshow{R:Chambers+Hastie:1992:c2}
}
\seealso{
  \code{\link{formula}}
}
\keyword{models}
