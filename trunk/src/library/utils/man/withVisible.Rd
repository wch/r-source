\name{withVisible}
\alias{withVisible}
\title{Return both a value and its visibility}
\description{
This function evaluates an expression, returning it in a two element list
containing its value and a flag showing whether it would automatically print.
}
\usage{
withVisible(x)
}
\arguments{
  \item{x}{ An expression to be evaluated. }
}
\details{
  The argument is evaluated in the caller's context.
}
\value{
  \item{value }{The value of \code{x} after evaluation.}
  \item{visible }{logical; whether the value would auto-print.}
}
\seealso{\code{\link{invisible}}, \code{\link{eval}}}
\examples{
x <- 1
withVisible(x <- 1)
x
withVisible(x)

# Wrap the call in evalq() for special handling

df <- data.frame(a=1:5, b=1:5)
evalq(withVisible(a + b), envir=df)
}
\keyword{programming}
