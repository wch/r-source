% File src/library/base/man/formals.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2023 R Core Team
% Distributed under GPL 2 or later

\name{formals}
\title{Access to and Manipulation of the Formal Arguments}
\alias{formals}
\alias{formals<-}
\description{
  Get or set the formal arguments of a \code{\link{function}}.
}
\usage{
formals(fun = sys.function(sys.parent()), envir = parent.frame())
formals(fun, envir = environment(fun)) <- value
}
\arguments{
  \item{fun}{a \code{\link{function}}, or see \sQuote{Details}.}
  \item{envir}{\code{\link{environment}} in which the function should be
    defined (or found via \code{\link{get}()} in the first case and when
    \code{fun} a character string).}
  \item{value}{a \code{\link{list}} (or \code{\link{pairlist}}, hence possibly
    \code{\link{NULL}}) of \R expressions.}
}
\details{
  For the first form, \code{fun} can also be a character string naming
  the function to be manipulated, which is searched for in \code{envir},
  by default from the parent
  frame.  If it is not specified, the function calling \code{formals} is
  used.

  Only \emph{closures}, i.e., non-primitive functions, have formals, not
  primitive functions.
  \cr
  Note that \code{formals(args(f))} gives a formal argument list for
  all functions \code{f}, primitive or not.
}
\value{
  \code{formals} returns the formal argument list of the function
  specified, as a \code{\link{pairlist}}, or \code{NULL} for a
  non-function or primitive.

  The replacement form sets the formals of a function to the
  list/pairlist on the right hand side, and (potentially) resets the
  environment of the function, dropping \code{\link{attributes}}.
}
\seealso{
  \code{\link{formalArgs}} (from \pkg{methods}), a shortcut for \code{names(formals(.))}.
  \code{\link{args}} for a human-readable version, \emph{and} as
  intermediary to get formals of a primitive function.
  \cr
  \code{\link{alist}} to \emph{construct} a typical formals \code{value},
  see the examples.

  The three parts of a (non-primitive) \code{\link{function}} are its
  \code{formals}, \code{\link{body}}, and \code{\link{environment}}.
}
\examples{
require(stats)
formals(lm)

## If you just want the names of the arguments, use formalArgs instead.
names(formals(lm))
methods:: formalArgs(lm)     # same

## formals returns a pairlist. Arguments with no default have type symbol (aka name).
str(formals(lm))

## formals returns NULL for primitive functions.  Use it in combination with
## args for this case.
is.primitive(`+`)
formals(`+`)
formals(args(`+`))

## You can overwrite the formal arguments of a function (though this is
## advanced, dangerous coding).
f <- function(x) a + b
formals(f) <- alist(a = , b = 3)
f    # function(a, b = 3) a + b
f(2) # result = 5
}
\keyword{programming}
