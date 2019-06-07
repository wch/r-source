% File src/library/base/man/stopifnot.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2019 R Core Team
% Distributed under GPL 2 or later

\name{stopifnot}
\title{Ensure the Truth of R Expressions}
\alias{stopifnot}
\concept{assertion}
\description{
  If any of the expressions (in \code{\dots} or \code{exprs}) are not
  \code{\link{all}} \code{TRUE}, \code{\link{stop}} is called, producing
  an error message indicating the \emph{first} expression which was not
  (\code{\link{all}}) true.
}
\usage{
stopifnot(\dots, exprs, exprObject, local = TRUE)
}
\arguments{
  \item{\dots, exprs}{any number of (typically but not necessarily
    \code{\link{logical}}) \R expressions, which should each evaluate to
    (a logical vector of all)
    \code{\link{TRUE}}.  Use \emph{either} \code{\dots} \emph{or}
    \code{exprs}, the latter typically an unevaluated expression of the
    form \preformatted{\{
   expr1
   expr2
   ....
\}}
  }
  \item{exprObject}{alternative to \code{exprs} or \code{...}: a
    an \sQuote{expression-like} object, typically an
    \code{\link{expression}}, but also a \code{\link{call}}, a
    \code{\link{name}}, or atomic constant such as \code{TRUE}.
  }
  \item{local}{(only when \code{exprs} is used:) indicates the
    \code{\link{environment}} in which the expressions should be
    evaluated; by default the one where \code{stopifnot()} has been called from.}
}
\details{
  This function is intended for use in regression tests or also argument
  checking of functions, in particular to make them easier to read.

  \code{stopifnot(A, B)} or equivalently \code{stopifnot(exprs= {A ;
      B})} are conceptually equivalent to \preformatted{ \{ if(any(is.na(A)) || !all(A)) stop(...);
   if(any(is.na(B)) || !all(B)) stop(...) \}}

  Since \R version 3.6.0, \code{stopifnot()} no longer handles potential
  errors or warnings (by \code{\link{tryCatch}()} etc) for each single
  expression % but currently uses a trick to suppress the *call* in the message
  and may use \code{\link{sys.call}(<n>)} to get a meaningful and short
  error message in case an expression did not evaluate to all TRUE.  This
  provides considerably less overhead.

  Since \R version 3.5.0, expressions \emph{are} evaluated sequentially,
  and hence evaluation stops as soon as there is a \dQuote{non-TRUE}, as
  indicated by the above conceptual equivalence statement.
  Further, when such an expression signals an error or
  \code{\link{warning}}, the message produced no longer
  contains the full \code{stopifnot} call, but just the erroneous
  expression.

  Also, since \R version 3.5.0, \code{stopifnot(exprs = { ... })} can be used
  alternatively and may be preferable in the case of several
  expressions, as they are more conveniently evaluated interactively
  (\dQuote{no extraneous \code{,} }).

  Since \R version 3.4.0, when an expression (from \code{\dots}) is not
  true \emph{and} is a call to \code{\link{all.equal}}, the error
  message will report the (first part of the) differences reported by
  \code{\link{all.equal}(*)}.
}
\note{
 Trying to use the \code{stopifnot(exprs = ..)} version via a shortcut,
 say, \preformatted{ assertWRONG <- function(exprs) stopifnot(exprs = exprs) }
 is delicate and the above is \emph{not a good idea}.  Contrary to \code{stopifnot()}
 which takes care to evaluate the parts of \code{exprs} one by one and
 stop at the first non-TRUE, the above short cut would typically evaluate
 all parts of \code{exprs} and pass the result, i.e., typically of the
 \emph{last} entry of \code{exprs} to \code{stopifnot()}.

 However, a more careful version, \preformatted{ assert <- function(exprs) eval.parent(substitute(stopifnot(exprs = exprs))) }
 may be a nice short cut for \code{stopifnot(exprs = *)} calls using the
 more commonly known verb as function name.
}
\value{
  (\code{\link{NULL}} if all statements in \code{\dots} are \code{TRUE}.)
}
\seealso{\code{\link{stop}}, \code{\link{warning}};
  \code{\link{assertCondition}} in package \pkg{tools} complements
  \code{stopifnot()} for testing warnings and errors.
}
\examples{
stopifnot(1 == 1, all.equal(pi, 3.14159265), 1 < 2) # all TRUE

m <- matrix(c(1,3,3,1), 2, 2)
stopifnot(m == t(m), diag(m) == rep(1, 2)) # all(.) |=>  TRUE

op <- options(error = expression(NULL))
# "disabling stop(.)"  << Use with CARE! >>

stopifnot(all.equal(pi, 3.141593),  2 < 2, all(1:10 < 12), "a" < "b")
## More convenient for interactive "line by line" evaluation:
stopifnot(exprs = {
  all.equal(pi, 3.1415927)
  2 < 2
  all(1:10 < 12)
  "a" < "b"
})

eObj <- expression(2 < 3, 3 <= 3:6, all(1:10 < 2))
stopifnot(exprObject = eObj)
stopifnot(exprObject = quote(3 == 3))
stopifnot(exprObject = TRUE)


# long all.equal() error messages are abbreviated:
stopifnot(all.equal(rep(list(pi),4), list(3.1, 3.14, 3.141, 3.1415)))

options(op)  # revert to previous error handler
}
\keyword{environment}
\keyword{programming}
\keyword{error}
