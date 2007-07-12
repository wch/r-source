% $Id: asOneSidedFormula.Rd,v 1.1 2003/12/11 07:16:06 ripley Exp $
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
  one-sided formulae. If \code{object} is a formula, it must be
  one-sided, in which case it is returned unaltered.
}
\value{
  a one-sided formula representing \code{object}
}
\author{Jose Pinheiro and Douglas Bates}

\seealso{\code{\link{formula}}}

\examples{
asOneSidedFormula("age")
asOneSidedFormula(~ age)
}
\keyword{models}
