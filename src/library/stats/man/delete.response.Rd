% File src/library/stats/man/delete.response.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{delete.response}
\title{Modify Terms Objects}
\usage{
delete.response(termobj)

reformulate(termlabels, response = NULL, intercept = TRUE)

drop.terms(termobj, dropx = NULL, keep.response = FALSE)
}
\alias{reformulate}
\alias{drop.terms}
\alias{delete.response}
\alias{[.terms}
\arguments{
  \item{termobj}{A \code{terms} object}
  \item{termlabels}{character vector giving the right-hand side of a
    model formula.  Cannot be zero-length.}
  \item{response}{character string, symbol or call giving the left-hand side of
    a model formula, or \code{NULL}.}
  \item{intercept}{logical: should the formula have an intercept?}
  \item{dropx}{vector of positions of variables to drop from the
    right-hand side of the model.}
  \item{keep.response}{Keep the response in the resulting object?}
}
\description{
  \code{delete.response} returns a \code{terms} object for the same
  model but with no response variable.

  \code{drop.terms} removes variables from the right-hand side of the
  model. There is also a \code{"[.terms"} method to perform the same
  function (with \code{keep.response = TRUE}).

  \code{reformulate} creates a formula from a character vector.
}
\value{
  \code{delete.response} and \code{drop.terms} return a \code{terms}
  object.

  \code{reformulate} returns a \code{formula}.
}
\seealso{\code{\link{terms}}}

\examples{
ff <- y ~ z + x + w
tt <- terms(ff)
tt
delete.response(tt)
drop.terms(tt, 2:3, keep.response = TRUE)
tt[-1]
tt[2:3]
reformulate(attr(tt, "term.labels"))

## keep LHS :
reformulate("x*w", ff[[2]])
fS <- surv(ft, case) ~ a + b
reformulate(c("a", "b*f"), fS[[2]])

## using non-syntactic names:
reformulate(c("`P/E`", "`\% Growth`"), response = as.name("+-"))

stopifnot(identical(      ~ var, reformulate("var")),
          identical(~ a + b + c, reformulate(letters[1:3])),
          identical(  y ~ a + b, reformulate(letters[1:2], "y"))
         )
}
\keyword{programming}
