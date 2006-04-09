% $Id: getInitial.Rd,v 1.1 2003/12/11 07:16:06 ripley Exp $
\name{getInitial}
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
\author{Jose Pinheiro and Douglas Bates}
\seealso{\code{\link{nls}}, %\code{\link{nlsList}},
    \code{\link{selfStart}},
    \code{\link{selfStart.default}}, \code{\link{selfStart.formula}}
}
\examples{
PurTrt <- Puromycin[ Puromycin$state == "treated", ]
getInitial( rate ~ SSmicmen( conc, Vm, K ), PurTrt )
}
\keyword{models}
\keyword{nonlinear}
\keyword{manip}
