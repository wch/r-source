\name{body}
\alias{body}
\alias{body<-}
\title{Access to and Manipulation of the Body of a Function}
\description{
  Get or set the body of a function.
}
\usage{
body(fun = sys.function(sys.parent()))
body(fun, envir = environment(fun)) <- value
}
\arguments{
  \item{fun}{a function object, or see Details.}
  \item{envir}{environment in which the function should be defined.}
  \item{value}{an expression or a list of \R expressions.}
}
\value{
  \code{body} returns the body of the function specified.

  The replacement form sets the body of a function to the
  expression/list on the right hand side, and (potentially) resets the
  environment of the function.
}
\details{
  For the first form, \code{fun} can be a character string
  naming the function to be manipulated, which is searched for from the
  parent environment.
  If it is not specified, the function calling \code{body} is used.
}
\seealso{
  \code{\link{alist}},
  \code{\link{args}},
  \code{\link{function}}.
}
\examples{
body(body)
f <- function(x) x^5
body(f) <- expression(5^x)
## or equivalently  body(f) <- list(quote(5^x))
f(3) # = 125
body(f)
}
\keyword{programming}
