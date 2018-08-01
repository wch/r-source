% File src/library/base/man/is.language.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2018 R Core Team
% Distributed under GPL 2 or later

\name{is.language}
\title{Is an Object a Language Object?}
\alias{is.language}
\alias{language}
\alias{language object}
\alias{language objects}
\usage{
is.language(x)
}
\description{
  \code{is.language} returns \code{TRUE} if \code{x} is a
  variable \code{\link{name}}, a \code{\link{call}}, or an
  \code{\link{expression}}.
}
\arguments{
  \item{x}{object to be tested.}
}
\note{
  A \code{name} is also known as \sQuote{symbol}, from its type
  (\code{\link{typeof}}), see \code{\link{is.symbol}}.

  If \code{\link{typeof}(x) == "language"}, then \code{is.language(x)}
  is always true, but the reverse does not hold as expressions or
  names \code{y} also fulfill \code{is.language(y)}, see the examples.

  This is a \link{primitive} function.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\examples{
ll <- list(a = expression(x^2 - 2*x + 1), b = as.name("Jim"),
           c = as.expression(exp(1)), d = call("sin", pi))
sapply(ll, typeof)
sapply(ll, mode)
stopifnot(sapply(ll, is.language))
}
\keyword{programming}
