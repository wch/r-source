% File src/library/base/man/function.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2012 R Core Team
% Distributed under GPL 2 or later

\name{function}
\alias{function}
\alias{return}
\alias{closure}

\title{Function Definition}
\usage{
\special{function( arglist ) expr}
\special{return(value)}
}
\description{
  These functions provide the base mechanisms for defining
  new functions in the \R language.
}
\arguments{
    \item{arglist}{Empty or one or more name or name=expression terms.}
    \item{expr}{An expression.}
    \item{value}{An expression.}
}
\details{
  The names in an argument list can be back-quoted non-standard names
  (see \sQuote{\link{backquote}}).

  If \code{value} is missing, \code{NULL} is returned.  If it is a
  single expression, the value of the evaluated expression is returned.
  (The expression is evaluated as soon as \code{return} is called, in
  the evaluation frame of the function and before any
  \code{\link{on.exit}} expression is evaluated.)

  If the end of a function is reached without calling \code{return}, the
  value of the last evaluated expression is returned.
}

\section{Technical details}{
  This type of function is not the only type in \R: they are called
  \emph{closures} (a name with origins in LISP) to distinguish them from
  \link{primitive} functions.

  A closure has three components, its \code{\link{formals}} (its argument
  list), its \code{\link{body}} (\code{expr} in the \sQuote{Usage}
  section) and its \code{\link{environment}} which provides the
  enclosure of the evaluation frame when the closure is used.

  There is an optional further component if the closure has been
  byte-compiled.  This is not normally user-visible, but it indicated
  when functions are printed.
}

\seealso{
  \code{\link{args}}.

  \code{\link{formals}}, \code{\link{body}} and
  \code{\link{environment}} for accessing the component parts of a
  function.

  \code{\link{debug}} for debugging; using \code{\link{invisible}} inside
  \code{return(.)} for returning \emph{invisibly}.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\examples{
norm <- function(x) sqrt(x\%*\%x)
norm(1:4)

## An anonymous function:
(function(x, y){ z <- x^2 + y^2; x+y+z })(0:7, 1)
}
\keyword{programming}
