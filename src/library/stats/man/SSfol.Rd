% File src/library/stats/man/SSfol.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2010 R Core Team
% Distributed under GPL 2 or later

\name{SSfol}
\encoding{UTF-8}
\title{Self-Starting Nls First-order Compartment Model}
\usage{
SSfol(Dose, input, lKe, lKa, lCl)
}
\alias{SSfol}
\arguments{
  \item{Dose}{a numeric value representing the initial dose.}
  \item{input}{a numeric vector at which to evaluate the model.}
  \item{lKe}{a numeric parameter representing the natural logarithm of
    the elimination rate constant.}
  \item{lKa}{a numeric parameter representing the natural logarithm of
    the absorption rate constant.}
  \item{lCl}{a numeric parameter representing the natural logarithm of
    the clearance.}
}
\description{
  This \code{selfStart} model evaluates the first-order compartment
  function and its gradient.  It has an \code{initial} attribute that
  creates initial estimates of the parameters \code{lKe}, \code{lKa},
  and \code{lCl}.
}
\value{
  a numeric vector of the same length as \code{input}, which is the
  value of the expression
\preformatted{Dose * exp(lKe+lKa-lCl) * (exp(-exp(lKe)*input) - exp(-exp(lKa)*input))
    / (exp(lKa) - exp(lKe))
}

  If all of the arguments \code{lKe}, \code{lKa}, and \code{lCl} are
  names of objects, the gradient matrix with respect to these names is
  attached as an attribute named \code{gradient}.
}
\author{\enc{José}{Jose} Pinheiro and Douglas Bates}
\seealso{\code{\link{nls}}, \code{\link{selfStart}}
}
\examples{
Theoph.1 <- Theoph[ Theoph$Subject == 1, ]
SSfol(Theoph.1$Dose, Theoph.1$Time, -2.5, 0.5, -3) # response only
lKe <- -2.5; lKa <- 0.5; lCl <- -3
SSfol(Theoph.1$Dose, Theoph.1$Time, lKe, lKa, lCl) # response and gradient
getInitial(conc ~ SSfol(Dose, Time, lKe, lKa, lCl), data = Theoph.1)
## Initial values are in fact the converged values
fm1 <- nls(conc ~ SSfol(Dose, Time, lKe, lKa, lCl), data = Theoph.1)
summary(fm1)
}
\keyword{models}
