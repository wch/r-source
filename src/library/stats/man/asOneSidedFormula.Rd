% File src/library/stats/man/asOneSidedFormula.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2010 R Core Team
% Distributed under GPL 2 or later

\name{asOneSidedFormula}
\encoding{UTF-8}
\title{Convert to One-Sided Formula}
\usage{
asOneSidedFormula(object)
}
\alias{asOneSidedFormula}
\arguments{
 \item{object}{a one-sided formula, an expression, a numeric value, or a
   character string.}
}
\description{
  Names, expressions, numeric values, and character strings are converted to
  one-sided formulae. If \code{object} is a formula, it must be
  one-sided, in which case it is returned unaltered.
}
\value{
  a one-sided formula representing \code{object}
}
\author{\enc{José}{Jose} Pinheiro and Douglas Bates}

\seealso{\code{\link{formula}}}

\examples{
asOneSidedFormula("age")
asOneSidedFormula(~ age)
}
\keyword{models}
