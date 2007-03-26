\name{substitute}
\title{Substituting and Quoting Expressions}
\usage{
substitute(expr, env)
quote(expr)
}
\alias{substitute}
\alias{quote}
\description{
  \code{substitute} returns the parse tree for the (unevaluated)
  expression \code{expr}, substituting any variables bound in
  \code{env}.

  \code{quote} simply returns its argument. The argument is not evaluated
  and can be any R expression.
}
\arguments{
  \item{expr}{Any syntactically valid \R expression}
  \item{env}{An environment or a list object.  Defaults to the
    current evaluation environment.}
}
\details{
  The typical use of \code{substitute} is to create informative labels
  for data sets and plots.
  The \code{myplot} example below shows a simple use of this facility.
  It uses the functions \code{\link{deparse}} and \code{substitute}
  to create labels for a plot which are character string versions
  of the actual arguments to the function \code{myplot}.

  Substitution takes place by examining each component of the parse tree
  as follows: If it is not a bound symbol in \code{env}, it is
  unchanged. If it is a promise object, i.e., a formal argument to a
  function or explicitly created using \code{\link{delayedAssign}()},
  the expression slot of the promise replaces the symbol. If it is an
  ordinary variable, its value is substituted, unless \code{env} is
  \code{\link{.GlobalEnv}} in which case the symbol is left unchanged.

  \code{substitute} is a primitive function so positional matching is
  used and names of supplied arguments are ignored.
}
\value{
  The \code{\link{mode}} of the result is generally \code{"call"} but
  may in principle be any type. In particular, single-variable
  expressions have mode \code{"name"} and constants have the
  appropriate base mode.
}
\note{
  Substitute works on a purely lexical basis. There is no guarantee
  that the resulting expression makes any sense.

  Substituting and quoting often causes confusion when the argument is
  \code{expression(\dots)}. The result is a call to the
  \code{\link{expression}} constructor function and needs to be evaluated
  with \code{\link{eval}} to give the actual expression object.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{missing}} for argument \dQuote{missingness},
  \code{\link{bquote}} for partial substitution,
  \code{\link{sQuote}} and \code{\link{dQuote}} for adding quotation
  marks to strings.
}
\examples{
(s.e <- substitute(expression(a + b), list(a = 1)))  #> expression(1 + b)
(s.s <- substitute( a + b,            list(a = 1)))  #> 1 + b
c(mode(s.e), typeof(s.e)) #  "call", "language"
c(mode(s.s), typeof(s.s)) #   (the same)
# but:
(e.s.e <- eval(s.e))          #>  expression(1 + b)
c(mode(e.s.e), typeof(e.s.e)) #  "expression", "expression"

substitute(x <- x + 1, list(x=1)) # nonsense

myplot <- function(x, y)
    plot(x, y, xlab=deparse(substitute(x)),
         ylab=deparse(substitute(y)))

## Simple examples about lazy evaluation, etc:

f1 <- function(x, y = x)             { x <- x + 1; y }
s1 <- function(x, y = substitute(x)) { x <- x + 1; y }
s2 <- function(x, y) { if(missing(y)) y <- substitute(x); x <- x + 1; y }
a <- 10
f1(a)# 11
s1(a)# 11
s2(a)# a
typeof(s2(a))# "symbol"
}
\keyword{programming}
\keyword{data}
