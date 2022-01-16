% File src/library/stats/man/asOneSidedFormula.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2022 R Core Team
% Distributed under GPL 2 or later

\name{asOneSidedFormula}
\encoding{UTF-8}
\title{Convert to One-Sided Formula}
\usage{
asOneSidedFormula(object)
}
\alias{asOneSidedFormula}
\arguments{
 \item{object}{a one-sided formula, name, call, expression, numeric value, or
   character string.}
}
\description{
  Names, calls, expressions (first element), numeric values, and
  character strings are converted to one-sided formulae
  associated with the global environment.
  If the input is a formula, it must be
  one-sided, in which case it is returned unaltered.
}
\value{
  a one-sided formula representing \code{object}
}
\author{\enc{José}{Jose} Pinheiro and Douglas Bates}

\seealso{\code{\link{formula}}}

\examples{
(form <- asOneSidedFormula("age"))
stopifnot(exprs = {
    identical(form, asOneSidedFormula(form))
    identical(form, asOneSidedFormula(as.name("age")))
    identical(form, asOneSidedFormula(expression(age)))
})
asOneSidedFormula(quote(log(age)))
asOneSidedFormula(1)
}
\keyword{models}
