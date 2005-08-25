\name{cbind2}
\alias{cbind2}
%\docType{methods} - here in same file
\alias{cbind2-methods}
\alias{cbind2,ANY,ANY-method}
\alias{cbind2,ANY,missing-method}
%
\title{Combine two Objects by Columns}% or Rows (once we add 'rbind2()')
\description{
  Combine two \dQuote{matrix-like} \R objects by columns.
  \code{cbind2()} is a (S4) generic function with default methods.

  Since \code{cbind2} is called by \code{\link{cbind}()}, this allows
  \code{cbind} to \dQuote{work} for formally classed (aka \sQuote{S4})
  objects by providing S4 methods for these objects.
}
\usage{
cbind2(x, y)
}
\arguments{
  \item{x}{any \R object, typically \dQuote{matrix-like}.}
  \item{y}{any \R object, typically similar to \code{x}, or missing
    completely.}
}
\section{Methods}{
  \describe{
    \item{x = "ANY", y = "ANY"}{the default method using \R's internal code.}
    \item{x = "ANY", y = "missing"}{the default method for one argument
      using \R's internal code.}
  }
}
%% The R code is currently in ../R/MethodsListClass.R + ../R/cbind-rbind.R
% \details{
% }
\value{
  A matrix (or matrix like object) combining the columns of \code{x} and
  \code{y}.
}
\seealso{\code{\link{cbind}}}
\examples{
cbind2(1:3, 4)
m <- matrix(3:8, 2,3, dimnames=list(c("a","b"), LETTERS[1:3]))
cbind2(1:2, m) # keeps dimnames from m

trace("cbind2")
methods:::cbind(a=1:3)# no call to cbind2()
methods:::cbind(a=1:3, four=4, 7:9)# calling cbind2() twice
untrace("cbind2")
}
\keyword{array}
\keyword{manip}
