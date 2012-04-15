% File src/library/stats/man/factor.scope.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{factor.scope}
\title{Compute Allowed Changes in Adding to or Dropping from a Formula}
\usage{
add.scope(terms1, terms2)

drop.scope(terms1, terms2)

factor.scope(factor, scope)
}
\alias{add.scope}
\alias{drop.scope}
\alias{factor.scope}
\arguments{
 \item{terms1}{the terms or formula for the base model.}
 \item{terms2}{the terms or formula for the upper (\code{add.scope}) or
   lower (\code{drop.scope}) scope. If missing for \code{drop.scope} it is
   taken to be the null formula, so all terms (except any intercept) are
   candidates to be dropped.}
 \item{factor}{the \code{"factor"} attribute of the terms of the base object.}
 \item{scope}{a list with one or both components \code{drop} and
   \code{add} giving the \code{"factor"} attribute of the lower and
   upper scopes respectively.}
}
\description{
 \code{add.scope} and \code{drop.scope} compute those terms that can be
 individually added to or dropped from a model while respecting the
 hierarchy of terms.
}
\details{
 \code{factor.scope} is not intended to be called directly by users.
}
\value{
  For \code{add.scope} and \code{drop.scope} a character vector of
  terms labels.  For \code{factor.scope}, a list with components
  \code{drop} and \code{add}, character vectors of terms labels.
}

\seealso{\code{\link{add1}}, \code{\link{drop1}},
  \code{\link{aov}}, \code{\link{lm}}}

\examples{
add.scope( ~ a + b + c + a:b,  ~ (a + b + c)^3)
# [1] "a:c" "b:c"
drop.scope( ~ a + b + c + a:b)
# [1] "c"   "a:b"
}
\keyword{models}
