\name{eval}
\title{Evaluate an (Unevaluated) Expression}
\usage{eval(expr, envir=sys.frame(sys.parent()))}
\alias{eval}
\arguments{
  \item{expr}{object of mode \code{\link{expression}} or an ``unevaluated
    expression''.}
  \item{envir}{the \code{\link{environment}} in which \code{expr} is to be
      evaluated.}
}
\description{
This function evaluates the expression \code{expr} argument in the
environment specified by \code{envir} and returns the computed value.  If
\code{envir} is not specified, then
\code{\link{sys.frame}(\link{sys.parent}())}, the environment where the
call to \code{eval} was made is used.
This allows you to assign complicated expressions to symbols and then
evaluate them. If you want to evaluate \code{expr} in \code{envir} then
you must wrap a call to \code{expression} around it.
}
\seealso{
\code{\link{expression}}, \code{\link{sys.frame}}, \code{\link{environment}}.
}
\examples{
eval(2 ^ 2 ^ 3)
mEx <- expression(2^2^3); mEx; 1 + eval(mEx)
eval({ xx <- pi; xx^2}) ; xx

ev <- function() {
	e1 <- sys.frame(sys.parent())
	## Evaluate a in e1
	aa <- eval(expression(a),e1)
	## evaluate the expression bound to a in e1
	a <- expression(x+y)
	list(aa = aa, eval = eval(a, e1))
}
tst.ev <- function(a = 7) { x <- pi; y <- 1; ev() }
tst.ev()#-> aa : 7,  eval : 4.14
}
\keyword{data}
\keyword{programming}
