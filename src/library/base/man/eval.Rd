\name{eval}
\alias{eval}
\alias{evalq}
\alias{eval.parent}
\alias{local}
\title{Evaluate an (Unevaluated) Expression}
\description{
  Evaluate an \R expression in a specified environment.
}
\usage{
eval(expr, envir = parent.frame(),
    	   enclos = if(is.list(envir) || is.pairlist(envir))
                       parent.frame() else baseenv())
evalq(expr, envir, enclos)
eval.parent(expr, n = 1)
local(expr, envir = new.env())
}
\arguments{
  \item{expr}{object of mode \code{\link{expression}} or \code{call} or
    an \dQuote{unevaluated expression}.}
  \item{envir}{the \code{\link{environment}} in which \code{expr} is to
    be evaluated.  May also be, \code{NULL}, a list, a data frame,
    or an integer as in \code{sys.call}.}
  \item{enclos}{Relevant when \code{envir} is a list or a data frame.
    Specifies the
    enclosure, i.e., where \R looks for objects not found in
    \code{envir}.}
  \item{n}{parent generations to go back}
}
\details{
  \code{eval} evaluates the expression \code{expr} argument in the
  environment specified by \code{envir} and returns the computed value.
  If \code{envir} is not specified, then
  \code{\link{sys.frame}(\link{sys.parent}())}, the environment where
  the call to \code{eval} was made is used.

  The \code{evalq} form is equivalent to \code{eval(quote(expr), \dots)}.

  As \code{eval} evaluates its first argument before passing it to the
  evaluator, it allows you to assign complicated expressions to symbols
  and then evaluate them.  \code{evalq} avoids this.

  \code{eval.parent(expr, n)} is a shorthand for \code{eval(expr,
    parent.frame(n))}.

  If \code{envir} is a data frame or list, it is copied into a temporary
  environment, and the copy is used for evaluation.  So if \code{expr}
  changes any of the components named in the data frame/list, the changes
  are lost.

  If \code{envir} is \code{NULL} it is treated as an empty list or data
  frame: no values will be found in \code{envir}, so look-up goes
  directly to \code{enclos}.

  A value of \code{NULL} for \code{enclos} is interpreted as the
  environment of the base package.
  
  
  \code{local} evaluates an expression in a local environment.  It is
  equivalent to \code{evalq} except that its default argument creates a
  new, empty environment.  This is useful to create anonymous recursive
  functions and as a kind of limited namespace feature since variables
  defined in the environment are not visible from the outside.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole. (\code{eval} only.)
}
\seealso{
  \code{\link{expression}}, \code{quote}, \code{\link{sys.frame}},
  \code{\link{parent.frame}}, \code{\link{environment}}.

  Further, \code{\link{force}} to \emph{force} evaluation, typically of
  function arguments.
}
\note{
  Due to the difference in scoping rules, there are some differences
  between \R and S in this area.  In particular, the default enclosure
  in S is the global environment.

  When evaluating expressions in data frames that has been passed as
  argument to a function, the relevant enclosure is often the caller's
  environment, i.e., one needs
  \code{eval(x, data, parent.frame())}.
}
\examples{
eval(2 ^ 2 ^ 3)
mEx <- expression(2^2^3); mEx; 1 + eval(mEx)
eval({ xx <- pi; xx^2}) ; xx

a <- 3 ; aa <- 4 ; evalq(evalq(a+b+aa, list(a=1)), list(b=5)) # == 10
a <- 3 ; aa <- 4 ; evalq(evalq(a+b+aa, -1), list(b=5))         # == 12

ev <- function() {
   e1 <- parent.frame()
   ## Evaluate a in e1
   aa <- eval(expression(a),e1)
   ## evaluate the expression bound to a in e1
   a <- expression(x+y)
   list(aa = aa, eval = eval(a, e1))
}
tst.ev <- function(a = 7) { x <- pi; y <- 1; ev() }
tst.ev()#-> aa : 7,  eval : 4.14

##
## Uses of local()
##

# Mutual recursives.
# gg gets value of last assignment, an anonymous version of f.

gg <- local({
    k <- function(y)f(y)
    f <- function(x) if(x) x*k(x-1) else 1
})
gg(10)
sapply(1:5, gg)

# Nesting locals. a is private storage accessible to k
gg <- local({
    k <- local({
        a <- 1
        function(y){print(a <<- a+1);f(y)}
    })
    f <- function(x) if(x) x*k(x-1) else 1
})
sapply(1:5, gg)

ls(envir=environment(gg))
ls(envir=environment(get("k", envir=environment(gg))))
}
\keyword{data}
\keyword{programming}
