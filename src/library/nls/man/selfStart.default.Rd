% $Id: selfStart.default.Rd,v 1.1.24.1 2001/03/24 00:08:06 bates Exp $
\name{selfStart.default}
\title{Construct Self-starting Nonlinear Models}
\usage{
selfStart(model, initial, parameters, template)
}
\alias{selfStart.default}
\arguments{
 \item{model}{a function object defining a nonlinear model.}
 \item{initial}{a function object, taking three arguments: \code{mCall},
   \code{data}, and \code{LHS}, representing, respectively, a matched
   call to the function \code{model}, a data frame in
   which to interpret the variables in \code{mCall}, and the expression
   from the left-hand side of the model formula in the call to \code{nls}.
   This function should return initial values for the parameters in
   \code{model}.}
 \item{parameters, template}{these arguments are included for
     consistency with the generic function, but are not used
     in the \code{default} method. See the documentation on
   \code{selfStart.formula}.}
}
\description{
  A method for the generic function `selfStart' for formula objects.
}
\value{
  a function object of class \code{selfStart}, corresponding to a
  self-starting nonlinear model function. An \code{initial} attribute
  (defined by the \code{initial} argument) is added to the function to
  calculate starting estimates for the parameters in the model
  automatically. 
}
\author{Jose Pinheiro and Douglas Bates}
\seealso{\code{\link{selfStart.formula}}}
\examples{
# `first.order.log.model' is a function object defining a first order
# compartment model 
# `first.order.log.initial' is a function object which calculates initial
# values for the parameters in `first.order.log.model'

# self-starting first order compartment model
\dontrun{
SSfol <- selfStart(first.order.log.model, first.order.log.initial)
}
}
\keyword{models}


