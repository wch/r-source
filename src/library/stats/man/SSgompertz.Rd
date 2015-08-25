% File src/library/stats/man/SSgompertz.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{SSgompertz}
\title{Self-Starting Nls Gompertz Growth Model}
\usage{
SSgompertz(x, Asym, b2, b3)
}
\alias{SSgompertz}
\arguments{
 \item{x}{a numeric vector of values at which to evaluate the model.}
 \item{Asym}{a numeric parameter representing the asymptote.}
 \item{b2}{a numeric parameter related to the value of the function at
   \code{x = 0}}
 \item{b3}{a numeric parameter related to the scale the \code{x} axis.}
}
\description{
  This \code{selfStart} model evaluates the Gompertz growth model
  and its gradient.  It has an \code{initial} attribute that
  creates initial estimates of the parameters \code{Asym},
  \code{b2}, and \code{b3}.
}
\value{
  a numeric vector of the same length as \code{input}.  It is the value of
  the expression \code{Asym*exp(-b2*b3^x)}.  If all of
  the arguments \code{Asym}, \code{b2}, and \code{b3} are
  names of objects the gradient matrix with respect to these names is attached as
  an attribute named \code{gradient}.
}
\author{Douglas Bates}
\seealso{\code{\link{nls}}, \code{\link{selfStart}}
}
\examples{
DNase.1 <- subset(DNase, Run == 1)
SSgompertz(log(DNase.1$conc), 4.5, 2.3, 0.7)   # response only
Asym <- 4.5; b2 <- 2.3; b3 <- 0.7
SSgompertz(log(DNase.1$conc), Asym, b2, b3) # response and gradient
print(getInitial(density ~ SSgompertz(log(conc), Asym, b2, b3),
                 data = DNase.1), digits = 5)
## Initial values are in fact the converged values
fm1 <- nls(density ~ SSgompertz(log(conc), Asym, b2, b3),
           data = DNase.1)
summary(fm1)
}
\keyword{models}
