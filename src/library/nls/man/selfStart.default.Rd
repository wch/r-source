% $Id: selfStart.default.Rd,v 1.3 2003/07/08 00:20:23 hornik Exp $
\name{selfStart.default}
\alias{selfStart.default}
\title{Construct Self-starting Nonlinear Models}
\description{
  A method for the generic function \code{selfStart} for formula objects.
}
\usage{
selfStart(model, initial, parameters, template)
}
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
   consistency with the generic function, but are not used in the
   \code{default} method.  See the documentation on
   \code{\link{selfStart.formula}}.}
}
\value{
  a function object of class \code{"selfStart"}, corresponding to a
  self-starting nonlinear model function.  An \code{initial} attribute
  (defined by the \code{initial} argument) is added to the function to
  calculate starting estimates for the parameters in the model
  automatically. 
}
\author{Jose Pinheiro and Douglas Bates}
\seealso{\code{\link{selfStart.formula}}}
\examples{
# 'first.order.log.model' is a function object defining a first order
# compartment model 
# 'first.order.log.initial' is a function object which calculates initial
# values for the parameters in 'first.order.log.model'

# self-starting first order compartment model
\dontrun{
SSfol <- selfStart(first.order.log.model, first.order.log.initial)
}
}
\keyword{models}


