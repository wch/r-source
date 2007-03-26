\name{splineDesign}
\alias{splineDesign}
\alias{spline.des}
\title{Design Matrix for B-splines}
\description{
  Evaluate the design matrix for the B-splines defined by \code{knots}
  at the values in \code{x}.
}
\usage{
splineDesign(knots, x, ord = 4, derivs, outer.ok = FALSE)
spline.des(knots, x, ord = 4, derivs, outer.ok = FALSE)
}
\arguments{
  \item{knots}{a numeric vector of knot positions with non-decreasing
    values.}
  \item{x}{a numeric vector of values at which to evaluate the B-spline
    functions or derivatives.  Unless \code{outer.ok} is truee, the
    values in \code{x} must be between \code{knots[ord]} and
    \code{knots[ length(knots) + 1 - ord ]}.}
  \item{ord}{a positive integer giving the order of the spline function.
    This is the number of coefficients in each piecewise polynomial
    segment, thus a cubic spline has order 4.  Defaults to 4.}
  \item{derivs}{an integer vector of the same length as \code{x} and with
    values between \code{0} and \code{ord - 1}.  The derivative of the
    given order is evaluated at the \code{x} positions.  Defaults to a
    vector of zeroes of the same length as \code{x}.}
  \item{outer.ok}{logical indicating if \code{x} should be allowed
    outside the \emph{inner} knots, see the \code{x} argument.}
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

knots <- c(1,1.8,3:5,6.5,7,8.1,9.2,10)# 10 => 10-4 = 6 Basis splines
x <- seq(min(knots)-1, max(knots)+1, length=501)
bb <- splineDesign(knots, x=x, outer.ok = TRUE)

plot(range(x), c(0,1), type="n", xlab="x", ylab="",
     main= "B-splines - sum to 1 inside inner knots")
mtext(expression(B[j](x) *"  and "* sum(B[j](x), j==1, 6)), adj=0)
abline(v=knots, lty=3, col="light gray")
abline(v=knots[c(4,length(knots)-3)], lty=3, col="gray10")
lines(x, rowSums(bb), col="gray", lwd=2)
matlines(x, bb, ylim = c(0,1), lty=1)
}
\keyword{ models }
