\name{predict.bs}
\title{Evaluate a predefined spline basis at new values}
\usage{
predict.bs(object, newx, \dots)
predict.ns(object, newx, \dots)
}
\alias{predict.bs}
\alias{predict.ns}
\arguments{
    \item{object}{the result of a call to \code{bs()} or \code{ns()}
	having attributes describing \code{knots}, \code{degree}, etc.}
    \item{newx}{the \code{x} values at which evaluations are required.}
    \item{\dots}{Optional additional arguments.  Presently no additional
    arguments are used.}
}
\value{
  an object just like \code{basis}, except evaluated at the new values
  of \code{x}.

  These are methods for the function \code{predict()} for objects
  inheriting from classes \code{bs} or \code{ns}.  See \code{predict}
  for the general behavior of this function.
}
\seealso{
  \code{bs}, \code{ns}, \code{poly}, \code{lo}, \code{s}
}
\examples{
library(splines)
data(women)
basis <- ns(women$height, df = 5)
newX <- seq(58, 72, len = 51)
# evaluate the basis at the new data
predict(basis, newX)
}
