\name{function}
\title{Function Definition}
\usage{
function( arglist ) expr
return(value)
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

  If the end of a function is reached without calling \code{return}, the
  value of the last evaluated expression is returned.
}
\section{Warning}{
  Prior to \R 1.8.0, \code{value} could be a series of non-empty
  expressions separated by commas.  In that case the value
  returned is a list of the evaluated expressions, with names set to
  the expressions where these are the names of \R objects. That is,
  \code{a=foo()} names the list component \code{a} and gives it the value
  which results from evaluating \code{foo()}.

  This has been deprecated (and a warning is given), as it was never
  documented in S, and whether or not the list is named differs by S versions.
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
  Wadsworth \& Brooks/Cole.
}
\examples{
norm <- function(x) sqrt(x\%*\%x)
norm(1:4)

## An anonymous function:
(function(x,y){ z <- x^2 + y^2; x+y+z })(0:7, 1)
}
\keyword{programming}
