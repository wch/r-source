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
\usage{
if(cond) expr
if(cond) cons.expr  else  alt.expr
for(var in seq) expr
while(cond) expr
repeat expr
break
next
}
\description{
  These are the basic control-flow constructs of the \R language.  They
  function in much the same way as control statements in any algol-like
  language.
}
\details{
  Note that \code{expr} and \code{cons.expr}, etc, in the Usage section
  above means an \emph{expression} in a formal sense.  This is either a
  simple expression or a so called \emph{compound expression}, usually
  of the form \code{\{ expr1 ; expr2 \}}.

  Note that it is a common mistake to forget putting braces (\code{\{ .. \}})
  around your statements, e.g., after \code{if(..)} or \code{for(....)}.
  In particular, you should not have a newline between \code{\}} and 
  \code{else} to avoid a syntax error in entering a \code{if ... else}
  construct at the keyboard or via \code{source}.
  For that reason, one (somewhat extreme) attitude of defensive programming
  uses braces always, e.g., for \code{if} clauses.

  The index \code{seq} in a \code{for} loop is evaluated at the start of
  the loop; changing it subsequently does not affect the loop.
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
