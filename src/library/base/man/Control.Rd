\name{Control}
\alias{Control}
\alias{if}
\alias{else}
\alias{for}
\alias{while}
\alias{repeat}
\alias{break}
\alias{next}
\title{Control Flow}
\description{
  These are the basic control-flow constructs of the \R language.  They
  function in much the same way as control statements in any Algol-like
  language.
}
\usage{
if(cond) expr
if(cond) cons.expr  else  alt.expr

for(var in seq) expr
while(cond) expr
repeat expr
break
next
}
\arguments{
  \item{cond}{A length-one logical vector that is not \code{NA}.
    Conditions of length greater than one are accepted with a warning, but
    only the first element is used.
  }
  \item{var}{A syntactical name for a variable.}
  \item{seq}{An expression evaluating to a vector (including a list).}
  \item{expr, cons.expr, alt.expr}{
    An \emph{expression} in a formal sense.  This is either a
    simple expression or a so called \emph{compound expression}, usually
    of the form \code{\{ expr1 ; expr2 \}}.
  }
}
\details{
  \code{break} breaks out of a \code{for}, \code{while} or \code{repeat}
  loop; control is transferred to the first statement outside the inner-most loop.
  \code{next} halts the processing of the current iteration and advances the
  looping index. Both \code{break} and \code{next} apply only to the innermost 
  of nested loops.
  
  Note that it is a common mistake to forget to put braces (\code{\{ .. \}})
  around your statements, e.g., after \code{if(..)} or \code{for(....)}.
  In particular, you should not have a newline between \code{\}} and 
  \code{else} to avoid a syntax error in entering a \code{if ... else}
  construct at the keyboard or via \code{source}.
  For that reason, one (somewhat extreme) attitude of defensive programming
  is to always use braces, e.g., for \code{if} clauses.

  The index \code{seq} in a \code{for} loop is evaluated at the start of
  the loop; changing it subsequently does not affect the loop.  The
  variable \code{var} has the same type as \code{seq}.  If \code{seq} is
  a factor (which is not strictly allowed) then its internal codes are
  used: the effect is that of \code{\link{as.integer}} not
  \code{\link{as.vector}}.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{Syntax}} for the basic \R syntax and operators,
  \code{\link{Paren}} for parentheses and braces; further,
  \code{\link{ifelse}}, \code{\link{switch}}.
}
\examples{
for(i in 1:5) print(1:i)
for(n in c(2,5,10,20,50)) {
   x <- rnorm(n)
   cat(n,":", sum(x^2),"\n")
}
}
\keyword{programming}
\keyword{iteration}
\keyword{logic}
