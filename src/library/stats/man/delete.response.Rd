% File src/library/stats/man/delete.response.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2025 R Core Team
% Distributed under GPL 2 or later

\name{delete.response}
\title{Modify Terms Objects}
\usage{
delete.response(termobj)

reformulate(termlabels, response = NULL, intercept = TRUE, env = parent.frame())

drop.terms(termobj, dropx = NULL, keep.response = FALSE)

\method{[}{terms}(termobj, i)
}
\alias{reformulate}
\alias{drop.terms}
\alias{delete.response}
\alias{[.terms}
\arguments{
  \item{termobj}{a \code{\link[=terms.object]{terms}} object.}
  \item{termlabels}{character vector giving the right-hand side of a
    model formula.  May be zero-length.}
  \item{response}{a character string, symbol or call giving the left-hand
    side of a model formula, or \code{NULL}.}
  \item{intercept}{logical: should the formula have an intercept?}
  \item{env}{the \code{\link{environment}} of the \code{\link{formula}}
    returned.}
  \item{dropx, i}{a numeric vector indexing \code{labels(termobj)}
    (that is, the \code{"term.labels"} attribute of \code{termobj}),
    indicating terms to drop (\code{dropx}) or select (\code{i}) from
    the right-hand side of the model. The default \code{dropx=NULL}
    means to keep all terms.}
  \item{keep.response}{logical: keep the response in the resulting object?}
}
\description{
  \code{delete.response} returns a \code{terms} object for the same
  model but with no response variable.

  \code{drop.terms} removes terms from the right-hand side of the
  model, optionally keeping the response.  There is also a
  \code{[.terms} method performing a complementary operation: it
  extracts terms, always keeping the response.

  \code{reformulate} creates a formula from a character vector.  If
  \code{length(termlabels) > 1}, its elements are concatenated with \code{+}.
  Non-syntactic names (e.g. containing spaces or special characters; see
  \code{\link{make.names}}) must be protected with backticks  (see examples).
  A non-\code{\link{parse}}able \code{response} still works for now,
  back compatibly, with a deprecation warning.
}
\value{
  \code{delete.response} and \code{drop.terms} return a \code{\link{terms}}
  object.

  \code{reformulate} returns a \code{\link{formula}}.
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

x <- c("a name", "another name")
tryCatch( reformulate(x), error = function(e) "Syntax error." )
## rather backquote the strings in x :
reformulate(sprintf("`\%s`", x))

stopifnot(identical(      ~ var, reformulate("var")),
          identical(~ a + b + c, reformulate(letters[1:3])),
          identical(  y ~ a + b, reformulate(letters[1:2], "y"))
         )
}
\keyword{programming}
