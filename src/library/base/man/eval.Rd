\name{eval}
\title{Evaluate an (Unevaluated) Expression}
\usage{
eval(expr, envir=sys.frame(sys.parent()), 
   enclos=if(is.list(envir) || is.pairlist(envir)) 
             sys.frame(sys.parent()))

evalq(expr, envir, enclos)
}
\alias{eval}
\alias{evalq}
\arguments{
  \item{expr}{object of mode \code{\link{expression}} or an ``unevaluated
    expression''.}
  \item{envir}{the \code{\link{environment}} in which \code{expr} is to be
      evaluated. May also be a list or an integer as in \code{sys.call}.}
  \item{enclos}{Only relevant if \code{envir} is a list. Specifies the
      enclosure, i.e. where R looks for objects not found in \code{envir}.} 
}
\description{
This function evaluates the expression \code{expr} argument in the
environment specified by \code{envir} and returns the computed value.  If
\code{envir} is not specified, then
\code{\link{sys.frame}(\link{sys.parent}())}, the environment where the
call to \code{eval} was made is used. The \code{eval} form evaluates its first
argument before passing it to the evaluator.
This allows you to assign complicated expressions to symbols and then
evaluate them. The \code{evalq} form is equivalent to
\code{eval(quote(expression), ...)} 
}
\seealso{
\code{\link{expression}}, \code{quote}, \code{\link{sys.frame}},
\code{\link{environment}}. 
}
\note{
Due to the difference in scoping rules, there are some differences
between R and S in this area. In particular, the default enclosure
in S is the global environment.

When evaluating expressions in dataframes that has been passed as
argument to a function, the relevant enclosure is often the caller's
environment, i.e. one needs 
\code{eval(x, data, sys.frame(sys.parent()))}.
}

\examples{
eval(2 ^ 2 ^ 3)
mEx <- expression(2^2^3); mEx; 1 + eval(mEx)
eval({ xx <- pi; xx^2}) ; xx

a <- 3 ; aa <- 4 ; evalq(evalq(a+b+aa, list(a=1)), list(b=5)) # == 10
a <- 3 ; aa <- 4 ; evalq(evalq(a+b+aa, 1), list(b=5))         # == 12

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
