% $Id: selfStart.formula.Rd,v 1.2 2001/03/24 06:57:57 ripley Exp $
\name{selfStart.formula}
\title{Construct Self-starting Nonlinear Models}
\usage{
selfStart(model, initial, parameters, template)
}
\alias{selfStart.formula}
\arguments{
    \item{model}{a nonlinear formula object of the form \code{~expression}.}
    \item{initial}{a function object, taking three arguments: \code{mCall},
	\code{data}, and \code{LHS}, representing, respectively, a matched
	call to the function \code{model}, a data frame in
	which to interpret the variables in \code{mCall}, and the expression
	from the left-hand side of the model formula in the call to \code{nls}.
	This function should return initial values for the parameters in
	\code{model}.}
    \item{parameters}{a character vector specifying the terms on the right
	hand side of \code{model} for which initial estimates should be
	calculated. Passed as the \code{namevec} argument to the \code{deriv}
	function.}
    \item{template}{an optional prototype for the calling sequence of the
	returned object, passed as the \code{function.arg} argument to the
	\code{deriv} function. By default, a template is generated with the
	covariates in \code{model} coming first and the parameters in
	\code{model} coming last in the calling sequence.}
}
\description{
  A method for the generic function `selfStart' for formula objects.
}
\value{
  a function object of class \code{selfStart}, obtained by applying
  \code{deriv} to the right hand side of the \code{model} formula. An
  \code{initial} attribute (defined by the \code{initial} argument) is
  added to the function to calculate starting estimates for the
  parameters in the model automatically.
}
\author{Jose Pinheiro and Douglas Bates}
\seealso{\code{\link{selfStart.default}}, \code{\link{deriv}}
}
\examples{
## self-starting logistic model

SSlogis <- selfStart(~ Asym/(1 + exp((xmid - x)/scal)),
  function(mCall, data, LHS)
  {
    xy <- sortedXyData(mCall[["x"]], LHS, data)
    if(nrow(xy) < 4) {
      stop("Too few distinct x values to fit a logistic")
    }
    z <- xy[["y"]]
    if (min(z) <= 0) { z <- z + 0.05 * max(z) } # avoid zeroes
    z <- z/(1.05 * max(z))		# scale to within unit height
    xy[["z"]] <- log(z/(1 - z))		# logit transformation
    aux <- coef(lm(x ~ z, xy))
    parameters(xy) <- list(xmid = aux[1], scal = aux[2])
    pars <- as.vector(coef(nls(y ~ 1/(1 + exp((xmid - x)/scal)), 
  			     data = xy, algorithm = "plinear")))
    value <- c(pars[3], pars[1], pars[2])
    names(value) <- mCall[c("Asym", "xmid", "scal")]
    value
  }, c("Asym", "xmid", "scal"))
}

\keyword{models}


