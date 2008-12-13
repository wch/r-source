% File src/library/base/man/body.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2008 R Core Development Team
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
  \item{value}{an object, usually a language object: see section
    \sQuote{Value}.}
}
\details{
  For the first form, \code{fun} can be a character string
  naming the function to be manipulated, which is searched for from the
  parent environment.  If it is not specified, the function calling
  \code{body} is used.

  To set the value to a list call the replacement function with
  \code{value} a length-one list containing the desired value.
}
\value{
  \code{body} returns the body of the function specified.  This is
  normally a language object, most often a call to \code{\{}, but it can
  also be an object (e.g. \code{pi}) to be the return value of the function.

  The replacement form sets the body of a function to the
  object on the right hand side, and (potentially) resets the
  environment of the function.  If \code{value} is of class
  \code{"\link{expression}"} or a length-one list, the first
  element is used as the body.  For expressions (but not lists) any
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
body(f) <- expression(5^x)
## or equivalently  body(f) <- list(quote(5^x))
f(3) # = 125
body(f)
}
\keyword{programming}
