% File src/library/base/man/stopifnot.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2023 R Core Team
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
  \item{\dots, exprs}{any number of \R expressions, which should each
    evaluate to (a logical vector of all) \code{\link{TRUE}}.  Use
    \emph{either} \code{\dots} \emph{or}
    \code{exprs}, the latter typically an unevaluated expression of the
    form \preformatted{\{
   expr1
   expr2
   ....
\}}
  Note that e.g., positive numbers are \emph{not} \code{TRUE}, even when
  they are coerced to \code{TRUE}, e.g., inside \code{if(.)} or in
  arithmetic computations in \R.

  If names are provided to \code{\dots}, they will be used in lieu of
  the default error message.
  }
  \item{exprObject}{alternative to \code{exprs} or \code{...}:
    an \sQuote{expression-like} object, typically an
    \code{\link{expression}}, but also a \code{\link{call}}, a
    \code{\link{name}}, or atomic constant such as \code{TRUE}.
  }
  \item{local}{(only when \code{exprs} is used:) indicates the
    \code{\link{environment}} in which the expressions should be
    evaluated; by default the one from where \code{stopifnot()} has been
    called.}
}
\details{
  This function is intended for use in regression tests or also argument
  checking of functions, in particular to make them easier to read.

  \code{stopifnot(A, B)} or equivalently \code{stopifnot(exprs= {A ;
      B})} are conceptually equivalent to \preformatted{ \{ if(any(is.na(A)) || !all(A)) stop(...);
   if(any(is.na(B)) || !all(B)) stop(...) \}}

  Since \R version 3.6.0, \code{stopifnot()} no longer handles potential
  errors or warnings (by \code{\link{tryCatch}()} etc) for each single
  expression
  and may use \code{\link{sys.call}(\var{n})} to get a meaningful and short
  error message in case an expression did not evaluate to all TRUE.  This
  provides considerably less overhead.

  Since \R version 3.5.0, expressions \emph{are} evaluated sequentially,
  and hence evaluation stops as soon as there is a \dQuote{non-TRUE}, as
  indicated by the above conceptual equivalence statement.
  %%__ Too expensive {tryCatch(), in R 3.5.x} :
  %% Further, when such an expression signals an error or
  %% \code{\link{warning}}, the message produced no longer
  %% contains the full \code{stopifnot} call, but just the erroneous
  %% expression.

  Also, since \R version 3.5.0, \code{stopifnot(exprs = { ... })} can be used
  alternatively and may be preferable in the case of several
  expressions, as they are more conveniently evaluated interactively
  (\dQuote{no extraneous \code{,} }).

  Since \R version 3.4.0, when an expression (from \code{\dots}) is not
  true \emph{and} is a call to \code{\link{all.equal}}, the error
  message will report the (first part of the) differences reported by
  \code{\link{all.equal}(*)}; since \R 4.3.0, this happens for all calls
  where \code{"all.equal"} \code{\link{pmatch}()}es the function called,
  e.g., when that is called \code{all.equalShow}, see the example in
  \code{\link{all.equal}}.
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
## NB: Some of these examples are expected to produce an error. To
##     prevent them from terminating a run with example() they are
##     piped into a call to try().

stopifnot(1 == 1, all.equal(pi, 3.14159265), 1 < 2) # all TRUE

m <- matrix(c(1,3,3,1), 2, 2)
stopifnot(m == t(m), diag(m) == rep(1, 2)) # all(.) |=>  TRUE

stopifnot(length(10)) |> try() # gives an error: '1' is *not* TRUE
## even when   if(1) "ok"   works

stopifnot(all.equal(pi, 3.141593),  2 < 2, (1:10 < 12), "a" < "b") |> try()
## More convenient for interactive "line by line" evaluation:
stopifnot(exprs = {
  all.equal(pi, 3.1415927)
  2 < 2
  1:10 < 12
  "a" < "b"
}) |> try()

eObj <- expression(2 < 3, 3 <= 3:6, 1:10 < 2)
stopifnot(exprObject = eObj) |> try()
stopifnot(exprObject = quote(3 == 3))
stopifnot(exprObject = TRUE)


# long all.equal() error messages are abbreviated:
stopifnot(all.equal(rep(list(pi),4), list(3.1, 3.14, 3.141, 3.1415))) |> try()

# The default error message can be overridden to be more informative:
m[1,2] <- 12
stopifnot("m must be symmetric"= m == t(m)) |> try()
#=> Error: m must be symmetric

##' warnifnot(): a "only-warning" version of stopifnot()
##'   {Yes, learn how to use do.call(substitute, ...) in a powerful manner !!}
warnifnot <- stopifnot ; N <- length(bdy <- body(warnifnot))
bdy        <- do.call(substitute, list(bdy,   list(stopifnot = quote(warnifnot))))
bdy[[N-1]] <- do.call(substitute, list(bdy[[N-1]], list(stop = quote(warning))))
body(warnifnot) <- bdy
warnifnot(1 == 1, 1 < 2, 2 < 2) # => warns " 2 < 2 is not TRUE  "
warnifnot(exprs = {
    1 == 1
    3 < 3  # => warns "3 < 3 is not TRUE"
})
}
\keyword{environment}
\keyword{programming}
\keyword{error}
