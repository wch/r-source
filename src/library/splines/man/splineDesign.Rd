\name{splineDesign}
\title{Design Matrix for B-splines}
\usage{
splineDesign(knots, x, ord, derivs)
spline.des(knots, x, ord, derivs)
}
\alias{splineDesign}
\alias{spline.des}
\arguments{
 \item{knots}{a numeric vector of knot positions with non-decreasing values.}
 \item{x}{a numeric vector of values at which to evaluate the B-spline
   functions or derivatives. The values in \code{x} must be between
   \code{knots[ord]} and \code{knots[ length(knots) + 1 - ord ]}.}
 \item{ord}{a positive integer giving the order of the spline function.
   This is the number of coefficients in each piecewise polynomial
   segment, thus a cubic spline has order 4.  Defaults to 4.}
 \item{derivs}{an integer vector of the same length as \code{x} and with
   values between \code{0} and \code{ord - 1}.  The derivative of the
   given order is evaluated at the \code{x} positions.  Defaults to a
   vector of zeroes of the same length as \code{x}.}
}
\description{
  Evaluate the design matrix for the B-splines defined by \code{knots}
  at the values in \code{x}.
}
\value{
  A matrix with \code{length( x )} rows and \code{length( knots ) - ord}
  columns.  The i'th row of the matrix contains the coefficients of the
  B-splines (or the indicated derivative of the B-splines) defined by
  the \code{knot} vector and evaluated at the i'th value of \code{x}.
  Each B-spline is defined by a set of \code{ord} successive knots so
  the total number of B-splines is \code{length(knots)-ord}.
}
\note{The older \code{spline.des} function takes the same arguments but
  returns a list with several components including \code{knots},
  \code{ord}, \code{derivs}, and \code{design}.  The \code{design}
  component is the same as the value of the \code{splineDesign}
  function.
}
\author{Douglas Bates and Bill Venables}
\examples{
splineDesign(knots = 1:10, x = 4:7)
}
\keyword{ models }
