% $Id: asOneSidedFormula.Rd,v 1.1 1999/11/12 13:34:35 bates Exp $
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
  Names, expressions, numeric values, and strings are converted to
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
