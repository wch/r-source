% File src/library/base/man/function.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2012 R Core Development Team
% Distributed under GPL 2 or later

\name{function}
\title{Function Definition}
\usage{
\special{function( arglist ) expr}
\special{return(value)}
}
\alias{function}
\alias{return}
\description{
  These functions provide the base mechanisms for defining
  new functions in the \R language.
}
\arguments{
    \item{arglist}{Empty or one or more name or name=expression terms.}
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
\seealso{
  \code{\link{args}} and \code{\link{body}} for accessing the arguments
  and body of a function.

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
(function(x,y){ z <- x^2 + y^2; x+y+z })(0:7, 1)
}
\keyword{programming}
