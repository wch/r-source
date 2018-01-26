% File src/library/base/man/stopifnot.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2018 R Core Team
% Distributed under GPL 2 or later

\name{stopifnot}
\title{Ensure the Truth of R Expressions}
\alias{stopifnot}
\alias{stopIfnot}
\concept{assertion}
\description{
  If any of the expressions (in \code{\dots} or \code{exprs}) are not
  \code{\link{all}} \code{TRUE}, \code{\link{stop}} is called, producing
  an error message indicating the \emph{first} expression which was not true.
}
\usage{
stopifnot(\dots)
stopIfnot(exprs, local = TRUE)
}
\arguments{
  \item{\dots, exprs}{any number of (\code{\link{logical}}) \R expressions,
    which should evaluate to \code{\link{TRUE}}.}
  \item{local}{indicates the \code{\link{environment}} in which the
    expressions should be evaluated; by default the one where
    \code{stopIfnot()} has been called from.}
}
\details{
  This function is intended for use in regression tests or also argument
  checking of functions, in particular to make them easier to read.

  \code{stopifnot(A, B)} or equivalently \code{stopIfnot({A ; B})} are conceptually equivalent to
\preformatted{ \{ if(any(is.na(A)) || !all(A)) stop(...);
   if(any(is.na(B)) || !all(B)) stop(...) \}}

  Since \R version 3.5.0, expressions \emph{are} evaluated sequentially,
  and hence evaluation stops as soon as there is a \dQuote{non-TRUE}, as
  indicated by the above conceptual equivalence statement.
  Further, when such an expression signals an error or
  \code{\link{warning}}, its \code{\link{conditionCall}()} no longer
  contains the full \code{stopifnot} call, but just the erroneous
  expression.

  Also, since \R version 3.5.0, \code{stopIfnot()} can be used
  alternatively and may be preferable in the case of several
  expressions, as they are more conveniently evaluated interactively
  (\dQuote{no extraneous \code{,} }).

  Since \R version 3.4.0, when an expression (from \code{\dots}) is not
  true \emph{and} is a call to \code{\link{all.equal}}, the error
  message will report the (first part of the) differences reported by
  \code{\link{all.equal}(*)}.
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
stopIfnot({
  all.equal(pi, 3.1415927)
  2 < 2
  all(1:10 < 12)
  "a" < "b"
})

# long all.equal() error messages are abbreviated:
stopifnot(all.equal(rep(list(pi),4), list(3.1, 3.14, 3.141, 3.1415)))

options(op)  # revert to previous error handler
}
\keyword{environment}
\keyword{programming}
\keyword{error}
