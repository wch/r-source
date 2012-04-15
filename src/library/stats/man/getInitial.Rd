% File src/library/stats/man/getInitial.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2010 R Core Team
% Distributed under GPL 2 or later

\name{getInitial}
\encoding{UTF-8}
\title{Get Initial Parameter Estimates}
\alias{getInitial}
\alias{getInitial.default}
\alias{getInitial.formula}
\alias{getInitial.selfStart}
\usage{
getInitial(object, data, \dots)
}
\arguments{
  \item{object}{a formula or a \code{selfStart} model that defines a
    nonlinear regression model}
  \item{data}{a data frame in which the expressions in the formula or
    arguments to the \code{selfStart} model can be evaluated}
  \item{\dots}{optional additional arguments}
}
\description{
  This function evaluates initial parameter estimates for a nonlinear
  regression model.  If \code{data} is a parameterized data frame or
  \code{pframe} object, its \code{parameters} attribute is returned.
  Otherwise the object is examined to see if it contains a call to a
  \code{selfStart} object whose \code{initial} attribute can be
  evaluated.
}
\value{
    A named numeric vector or list of starting estimates for the
    parameters.  The construction of many \code{selfStart} models is such
    that these "starting" estimates are, in fact, the converged parameter
    estimates.
}
\author{\enc{José}{Jose} Pinheiro and Douglas Bates}
\seealso{\code{\link{nls}}, %\code{\link{nlsList}},
    \code{\link{selfStart}},
    \code{\link{selfStart.default}}, \code{\link{selfStart.formula}}
}
\examples{
PurTrt <- Puromycin[ Puromycin$state == "treated", ]
print(getInitial( rate ~ SSmicmen( conc, Vm, K ), PurTrt ), digits = 3)
}
\keyword{models}
\keyword{nonlinear}
\keyword{manip}
