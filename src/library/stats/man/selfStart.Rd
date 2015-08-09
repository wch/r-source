% File src/library/stats/man/selfStart.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{selfStart}
\encoding{UTF-8}
\alias{selfStart}
\alias{selfStart.default}
\alias{selfStart.formula}
\title{Construct Self-starting Nonlinear Models}
\description{
  Construct self-starting nonlinear models.
}
\usage{
selfStart(model, initial, parameters, template)
}
\arguments{
  \item{model}{a function object defining a nonlinear model or
    a nonlinear formula object of the form \code{~expression}.}
 \item{initial}{a function object, taking three arguments: \code{mCall},
   \code{data}, and \code{LHS}, representing, respectively, a matched
   call to the function \code{model}, a data frame in
   which to interpret the variables in \code{mCall}, and the expression
   from the left-hand side of the model formula in the call to \code{nls}.
   This function should return initial values for the parameters in
   \code{model}.}
  \item{parameters}{a character vector specifying the terms on the right
    hand side of \code{model} for which initial estimates should be
    calculated.  Passed as the \code{namevec} argument to the
    \code{deriv} function.}
  \item{template}{an optional prototype for the calling sequence of the
    returned object, passed as the \code{function.arg} argument to the
    \code{deriv} function.  By default, a template is generated with the
    covariates in \code{model} coming first and the parameters in
    \code{model} coming last in the calling sequence.}
}
\details{
  This function is generic; methods functions can be written to handle
  specific classes of objects.
}
\value{
  a function object of class \code{"selfStart"}, for the \code{formula}
  method obtained by applying
  \code{deriv} to the right hand side of the \code{model} formula.  An
  \code{initial} attribute (defined by the \code{initial} argument) is
  added to the function to calculate starting estimates for the
  parameters in the model automatically.
}
\author{\enc{José}{Jose} Pinheiro and Douglas Bates}

\seealso{
  \code{\link{nls}}, \code{\link{getInitial}}.
  Each of the following are \code{"selfStart"} models (with examples)
\code{\link{SSasymp}}, \code{\link{SSasympOff}}, \code{\link{SSasympOrig}},
\code{\link{SSbiexp}}, \code{\link{SSfol}}, \code{\link{SSfpl}},
\code{\link{SSgompertz}}, \code{\link{SSlogis}}, \code{\link{SSmicmen}},
\code{\link{SSweibull}}

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
    z <- z/(1.05 * max(z))              # scale to within unit height
    xy[["z"]] <- log(z/(1 - z))         # logit transformation
    aux <- coef(lm(x ~ z, xy))
    parameters(xy) <- list(xmid = aux[1], scal = aux[2])
    pars <- as.vector(coef(nls(y ~ 1/(1 + exp((xmid - x)/scal)),
                             data = xy, algorithm = "plinear")))
    setNames(c(pars[3], pars[1], pars[2]),
             mCall[c("Asym", "xmid", "scal")])
  }, c("Asym", "xmid", "scal"))

# 'first.order.log.model' is a function object defining a first order
# compartment model
# 'first.order.log.initial' is a function object which calculates initial
# values for the parameters in 'first.order.log.model'

# self-starting first order compartment model
\dontrun{
SSfol <- selfStart(first.order.log.model, first.order.log.initial)
}

## Explore the self-starting models already available in R's  "stats":
pos.st <- which("package:stats" == search())
mSS <- apropos("^SS..", where = TRUE, ignore.case = FALSE)
(mSS <- unname(mSS[names(mSS) == pos.st]))
fSS <- sapply(mSS, get, pos = pos.st, mode = "function")
all(sapply(fSS, inherits, "selfStart"))  # -> TRUE

## Show the argument list of each self-starting function:
str(fSS, give.attr = FALSE)
}
\keyword{models}
