% File src/library/stats/man/numericDeriv.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2008 R Core Team
% Distributed under GPL 2 or later

\name{numericDeriv}
\alias{numericDeriv}
\title{Evaluate Derivatives Numerically}
\description{
    \code{numericDeriv} numerically evaluates the gradient of an expression.
}
\usage{
numericDeriv(expr, theta, rho = parent.frame(), dir = 1.0)
}
\arguments{
  \item{expr}{The expression to be differentiated.  The value of this
    expression should be a numeric vector.}
  \item{theta}{A character vector of names of numeric variables
    used in \code{expr}.}
  \item{rho}{An environment containing all the variables needed to
    evaluate \code{expr}.}
  \item{dir}{A numeric vector of directions to use for the finite
    differences.}
}
\details{
  This is a front end to the C function \code{numeric_deriv}, which is
  described in \emph{Writing R Extensions}.

  The numeric variables must be of type \code{real} and not \code{integer}.
}
\value{
  The value of \code{eval(expr, envir = rho)} plus a matrix
  attribute called \code{gradient}.  The columns of this matrix are
  the derivatives of the value with respect to the variables listed in
  \code{theta}.
}
\author{Saikat DebRoy \email{saikat@stat.wisc.edu}}
\examples{
\dontshow{od <- options(digits = 4)}
myenv <- new.env()
assign("mean", 0., envir = myenv)
assign("sd", 1., envir = myenv)
assign("x", seq(-3., 3., len = 31), envir = myenv)
numericDeriv(quote(pnorm(x, mean, sd)), c("mean", "sd"), myenv)
\dontshow{options(od)}
}
\keyword{models}

