% $Id: selfStart.Rd,v 1.2 2001/08/13 21:41:48 ripley Exp $
\name{selfStart}
\title{Construct Self-starting Nonlinear Models}
\usage{
selfStart(model, initial, parameters, template)
}
\alias{selfStart}
\description{
  This function is generic; methods functions can be written to handle
  specific classes of objects. Available methods include
  \code{selfStart.default} and \code{selfStart.formula}.
  See the documentation on the appropriate method function.
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
 \item{parameters, template}{arguments used by some methods..}
}
\value{
  a function object of the \code{selfStart} class.
}
\author{Jose Pinheiro and Douglas Bates}

\seealso{\code{\link{selfStart.default}}, \code{\link{selfStart.formula}}
}
\examples{
## see documentation for the methods
}    
\keyword{models}
