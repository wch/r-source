\name{body}
\alias{body}
\alias{body<-}
\title{Access to and Manipulation of the Body of a Function}
\description{
  Get or set the body of a function.
}
\usage{
body(fun = sys.function(sys.parent()))
body(fun, envir = parent.frame()) <- value
}
\arguments{
  \item{fun}{a function object or a character string naming the function
    to be manipulated.
    If not specified, the function calling \code{body} is used.}
  \item{envir}{environment in which the function should be defined.}
  \item{value}{a list of \R expressions.}
}
\value{
  \code{body} returns the body of the function specified.

  The assignment form sets the body of a function to the list on the
  right hand side.
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
f(3) # = 125
str(body(f))
}
\keyword{programming}
