\name{terms.object}
\title{Description of Terms Objects}
\description{
  An object of class \code{\link{terms}} holds information about a model.
  Usually the model was specified in terms of a \code{\link{formula}}
  and that formula was used to determine the terms object. 

  The object itself is simply the formula supplied to the call of
  \code{\link{terms.formula}}. The object has a number of attributes
  and they are used to construct the model frame.
}
\value{
  An object with the following attributes:
  \item{factors}{A matrix of variables by terms showing which variables
    appear in which terms.}
  \item{intercept}{Either 0, indicating no intercept is to be fit, or 1
    indicating that an intercept is to be fit.}
  \item{order}{An integer specifying the order (highest number of
    interactions) in the model.}
  \item{response} {The index of the variable (in variables) of the
  response (the left hand side of the formula).}
  \item{term.labels}{A character vector containing the labels for each
    of the terms in the model.}
  \item{variables}{A list of the variables in the model}
}
\note{
  These objects are different from those found in S. In particular there
  is no \code{formula} attribute, instead the object is itself a
  formula. Thus, the mode of a terms object is different as well.
\seealso{
\code{\link{terms}}, \code{\link{terms.default}}, \code{\link{formula}}.
}
%%--- MM: I would really like instructive examples here...
\keyword{models}