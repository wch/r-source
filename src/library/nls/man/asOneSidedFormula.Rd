% $Id: asOneSidedFormula.Rd,v 1.2 2001/02/07 19:13:08 pd Exp $
\name{asOneSidedFormula}
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
  one-sided formulas. If \code{object} is a formula, it must be
  one-sided, in which case it is returned unaltered.
}
\value{
  a one-sided formula representing \code{object}
}
\author{Jose Pinheiro and Douglas Bates}

\seealso{\code{\link{formula}}}

\examples{
library(nls)
asOneSidedFormula("age")
asOneSidedFormula(~ age)
}
\keyword{models}
