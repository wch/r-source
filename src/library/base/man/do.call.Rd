\name{do.call}
\title{Execute a Function Call}
\usage{
do.call(what, args, quote=FALSE)
}
\alias{do.call}
\arguments{
  \item{what}{either a function or a character string naming the function to 
be called.}
  \item{args}{a \emph{list} of arguments to the function call.  The
    \code{names} attribute of \code{args} gives the argument names.}
  \item{quote}{a logical value indicating whether to quote the arguments.}
}
\description{
  \code{do.call} constructs and executes a function call from the name
  of the function and a list of arguments to be passed to it.
  
  If \code{quote} is \code{FALSE}, the default, then the arguments are
  evaluated. If \code{quote} is \code{TRUE} then each argument is quoted
  (see \code{\link{quote}}) so that the effect of argument evaluation
  is to remove the quote - leaving the original argument unevaluated.

  The behavior of some functions, such as \code{\link{substitute}},
  will not be the same for functions evaluated using \code{do.call} as
  if they were evaluated from the interpretor. The precise semantics
  are currently undefined and subject to change.
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

do.call(paste, list(as.name("A"), as.name("B")), quote=TRUE)
}
\keyword{programming}
