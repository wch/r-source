\name{do.call}
\title{Execute a Function Call}
\usage{
do.call(what, args)
}
\alias{do.call}
\arguments{
  \item{what}{a character string naming the function to be called.}
  \item{args}{a \emph{list} of arguments to the function call.  The
    \code{names} attribute of \code{args} gives the argument names.}
}
\description{
  \code{do.call} constructs and executes a function call from the name
  of the function and a list of arguments to be passed to it.
}
\value{
  The result of the (evaluated) function call.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{call}} which creates an unevaluated call.
}
\examples{
do.call("complex", list(imag = 1:3))

## if we already have a list (e.g. a data frame)
## we need c() to add further arguments
tmp <- expand.grid(letters[1:2], 1:3, c("+", "-"))
do.call("paste", c(tmp, sep=""))
}
\keyword{programming}
