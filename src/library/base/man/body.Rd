% File src/library/base/man/body.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

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
  \item{fun}{a function object, or see \sQuote{Details}.}
  \item{envir}{environment in which the function should be defined.}
  \item{value}{an object, usually a \link{language object}: see section
    \sQuote{Value}.}
}
\details{
  For the first form, \code{fun} can be a character string
  naming the function to be manipulated, which is searched for from the
  parent frame.  If it is not specified, the function calling
  \code{body} is used.

  The bodies of all but the simplest are braced expressions, that is
  calls to \code{\{}: see the \sQuote{Examples} section for how to
  create such a call.
}
\value{
  \code{body} returns the body of the function specified.  This is
  normally a \link{language object}, most often a call to \code{\{}, but
  it can also be an object (e.g. \code{pi}) to be the return value of
  the function.

  The replacement form sets the body of a function to the
  object on the right hand side, and (potentially) resets the
  environment of the function.  If \code{value} is of class
  \code{"\link{expression}"} the first element is used as the body:  any
  additional elements are ignored, with a warning.
}
\seealso{
  \code{\link{alist}},
  \code{\link{args}},
  \code{\link{function}}.
}
\examples{
body(body)
f <- function(x) x^5
body(f) <- quote(5^x)
## or equivalently  body(f) <- expression(5^x)
f(3) # = 125
body(f)

## creating a multi-expression body
e <- expression(y <- x^2, return(y)) # or a list
body(f) <- as.call(c(as.name("{"), e))
f
f(8)
}
\keyword{programming}
