% File src/library/base/man/Control.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2017 R Core Team
% Distributed under GPL 2 or later

\name{Control}
\alias{Control}
\alias{if}
\alias{else}
\alias{for}
\alias{in}
\alias{while}
\alias{repeat}
\alias{break}
\alias{next}
\title{Control Flow}
\description{
  These are the basic control-flow constructs of the \R language.  They
  function in much the same way as control statements in any Algol-like
  language.  They are all \link{reserved} words.
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
    Conditions of length greater than one are currently accepted with a
    warning, but only the first element is used.  An error is signalled
    instead when the environment variable \env{_R_CHECK_LENGTH_1_CONDITION_}
    is set to true.  Other types are coerced to logical
    if possible, ignoring any class.
  }
  \item{var}{A syntactical name for a variable.}
  \item{seq}{An expression evaluating to a vector (including a list and
    an \link{expression}) or to a \link{pairlist} or \code{NULL}.  A
    factor value will be coerced to a character vector.}
  \item{expr, cons.expr, alt.expr}{
    An \emph{expression} in a formal sense.  This is either a
    simple expression or a so called \emph{compound expression}, usually
    of the form \code{\{ expr1 ; expr2 \}}.
  }
}
\details{
  \code{break} breaks out of a \code{for}, \code{while} or \code{repeat}
  loop; control is transferred to the first statement outside the
  inner-most loop. \code{next} halts the processing of the current
  iteration and advances the looping index.  Both \code{break} and
  \code{next} apply only to the innermost of nested loops.

  Note that it is a common mistake to forget to put braces (\code{\{ .. \}})
  around your statements, e.g., after \code{if(..)} or \code{for(....)}.
  In particular, you should not have a newline between \code{\}} and
  \code{else} to avoid a syntax error in entering a \code{if ... else}
  construct at the keyboard or via \code{source}.
  For that reason, one (somewhat extreme) attitude of defensive programming
  is to always use braces, e.g., for \code{if} clauses.

  The \code{seq} in a \code{for} loop is evaluated at the start of
  the loop; changing it subsequently does not affect the loop.  If
  \code{seq} has length zero the body of the loop is skipped. Otherwise the
  variable \code{var} is assigned in turn the value of each element of
  \code{seq}. You can assign to \code{var} within the body of the loop,
  but this will not affect the next iteration.  When the loop terminates,
  \code{var} remains as a variable containing its latest value.

}
\value{
  \code{if} returns the value of the expression evaluated, or
  \code{NULL} invisibly if none was (which may happen if there is no
  \code{else}).

  \code{for}, \code{while} and \code{repeat} return \code{NULL} invisibly.
  \code{for} sets \code{var} to the last used element of \code{seq},
  or to \code{NULL} if it was of length zero.

  \code{break} and \code{next} do not return a value as they transfer
  control within the loop.
}

\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{Syntax}} for the basic \R syntax and operators,
  \code{\link{Paren}} for parentheses and braces.

  \code{\link{ifelse}}, \code{\link{switch}} for other ways to control flow.
}
\examples{
for(i in 1:5) print(1:i)
for(n in c(2,5,10,20,50)) {
   x <- stats::rnorm(n)
   cat(n, ": ", sum(x^2), "\n", sep = "")
}
f <- factor(sample(letters[1:5], 10, replace = TRUE))
for(i in unique(f)) print(i)
}
\keyword{programming}
\keyword{iteration}
\keyword{logic}
