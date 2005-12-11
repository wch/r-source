\name{nlsModel}
\alias{nlsModel}
\alias{nlsModel.plinear}
\title{Create an nlsModel Object}
\usage{
nlsModel(form, data, start, wts)
}
\arguments{
 \item{form}{a nonlinear model formula.}
 \item{data}{a data frame or a list in which to evaluate the variables
     from the model formula.}
 \item{start}{a named list or named numeric vector of starting
   estimates for the parameters in the model.}
 \item{wts}{case weights.  If missing, assumed to be 1.}
}
\description{
  This is the constructor for \code{nlsModel} objects, which are
  function closures for several functions in a list.  The closure
  includes a nonlinear model formula, data values for the formula, as
  well as parameters and their values.
}
\details{
  An \code{nlsModel} object is primarily used within the \code{nls}
  function.  It encapsulates the model, the data, and the parameters in
  an environment and provides several methods to access characteristics
  of the model.  It forms an important component of the object returned
  by the \code{nls} function.

  See \code{\link{nls}} for where elements of the formula \code{form}
  are looked for.  In normal use all the variables will be in \code{data}.
}
\value{
    The value is a list of functions that share a common environment.
    \item{resid}{returns the residual vector evaluated at the current
	parameter values}
    \item{fitted}{returns the fitted responses and their gradient at the
	current parameter values}
    \item{formula}{returns the model formula}
    \item{deviance}{returns the residual sum-of-squares at the current
	parameter values}
    \item{gradient}{returns the gradient of the model function at the
	current parameter values}
    \item{conv}{returns the relative-offset convergence criterion
	evaluated at the current parmeter values}
    \item{incr}{returns the parameter increment calculated according to
	the Gauss-Newton formula}
    \item{setPars}{a function with one argument, \code{pars}.  It sets the
	parameter values for the \code{nlsModel} object and returns a
	logical value denoting a singular gradient array.}
    \item{getPars}{returns the current value of the model parameters as a
	numeric vector}
    \item{getAllPars}{returns the current value of the model parameters as a
	numeric vector}
    \item{getEnv}{returns the environment shared by these functions,
      which contains copies of all the variables in \code{data} and has
      as parent the environment of \code{form}.}
    \item{trace}{the function that is called at each iteration if
	tracing is enabled}
    \item{Rmat}{the upper triangular factor of the gradient array at the
	current parameter values}
    \item{predict}{takes as argument \code{newdata},a \code{data.frame} and
	returns the predicted response for \code{newdata}.}
}
\references{
  Bates, D.M. and Watts, D.G. (1988), \emph{Nonlinear Regression Analysis
    and Its Applications}, Wiley
}
\author{Douglas M. Bates and Saikat DebRoy}
\seealso{
  \code{\link{nls}}
}
\examples{
DNase1 <- DNase[ DNase$Run == 1, ]
mod <-
 nlsModel(density ~ SSlogis( log(conc), Asym, xmid, scal ),
          DNase1, list( Asym = 3, xmid = 0, scal = 1 ))
mod$getPars()     # returns the parameters as a list
mod$deviance()    # returns the residual sum-of-squares
mod$resid()       # returns the residual vector and the gradient
mod$incr()        # returns the suggested increment
mod$setPars( unlist(mod$getPars()) + mod$incr() )  # set new parameter values
mod$getPars()     # check the parameters have changed
mod$deviance()    # see if the parameter increment was successful
mod$trace()       # check the tracing
mod$Rmat()        # R matrix from the QR decomposition of the gradient
}
\keyword{nonlinear}
\keyword{models}
