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
  \item{expr}{an object to be evaluated.  See Details.}
  \item{envir}{the \code{\link{environment}} in which \code{expr} is to
    be evaluated.  May also be \code{NULL}, a list, a data frame,
    a pairlist or an integer as specified to \code{\link{sys.call}}.}
  \item{enclos}{Relevant when \code{envir} is a (pair)list or a data frame.
    Specifies the enclosure, i.e., where \R looks for objects not found
    in \code{envir}.  This can be \code{NULL} (interpreted as the base
    package environment) or an environment.}
  \item{n}{number of parent generations to go back}
}
\details{
  \code{eval} evaluates the \code{expr} argument in the
  environment specified by \code{envir} and returns the computed value.
  If \code{envir} is not specified, then the default is
  \code{\link{parent.frame}()} (the environment where the call to
  \code{eval} was made).

  Objects to be evaluated can be of types \code{\link{call}} or
  \code{\link{expression}} or \link{symbol} (when the symbol is looked
  up in the current scope and its binding is evaluated), a \link{promise}
  or any of the basic types such as vectors, functions and environments
  (which are returned unchanged).

  The \code{evalq} form is equivalent to \code{eval(quote(expr), \dots)}.
  \code{eval} evaluates its first argument in the current scope
  before passing it to the evaluator: \code{evalq} avoids this.

  \code{eval.parent(expr, n)} is a shorthand for
  \code{eval(expr, parent.frame(n))}.

  If \code{envir} is a list (such as a data frame) or pairlist, it is
  copied into a temporary environment (with enclosure \code{enclos}),
  and the temporary environment is used for evaluation.  So if
  \code{expr} changes any of the components named in the (pair)list, the
  changes are lost.

  If \code{envir} is \code{NULL} it is interpreted as an empty list so
  no values could be found in \code{envir} and look-up goes directly to
  \code{enclos}.
  
  \code{local} evaluates an expression in a local environment.  It is
  equivalent to \code{evalq} except that its default argument creates a
  new, empty environment.  This is useful to create anonymous recursive
  functions and as a kind of limited name space feature since variables
  defined in the environment are not visible from the outside.
}
\value{
  The result of evaluating the object: for an expression vector this it
  the result of evaluating the last elements.
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
a <- 3 ; aa <- 4 ; evalq(evalq(a+b+aa, -1), list(b=5))        # == 12

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

a <- list(a=3, b=4)
with(a, a <- 5) # alters the copy of a from the list, discarded.

##
## Example of evalq()
##

N <- 3
env <- new.env()
assign("N", 27, envir=env)
## this version changes the visible copy of N only, since the argument
## passed to eval is '4'.
eval(N <- 4, env)
N
get("N", envir=env)
## this version does the assignment in env, and changes N only there.
evalq(N <- 5, env)
N
get("N", envir=env)


##
## Uses of local()
##

# Mutually recursive.
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
