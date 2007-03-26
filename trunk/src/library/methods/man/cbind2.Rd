\name{cbind2}
\alias{cbind2}
\alias{rbind2}
%\docType{methods} - here in same file
\alias{cbind2-methods}
\alias{cbind2,ANY,ANY-method}
\alias{cbind2,ANY,missing-method}
\alias{rbind2-methods}
\alias{rbind2,ANY,ANY-method}
\alias{rbind2,ANY,missing-method}
%
\title{Combine two Objects by Columns or Rows}
\description{
  Combine two \dQuote{matrix-like} \R objects by columns (\code{cbind2})
  or rows (\code{rbind2}). These are (S4) generic functions with default
  methods.
}
\details{
  The main use of \code{cbind2} (\code{rbind2}) is to be called by
  \code{\link{cbind}()}  (\code{rbind()})
  \bold{if} these are activated.  This allows \code{cbind}
  (\code{rbind}) to \dQuote{work} for formally classed (aka \sQuote{S4})
  objects by providing S4 methods for these objects.  Currently, a call\cr
  \code{methods:::bind_activation(TRUE)}\cr
  is needed to install a
  \dQuote{\code{cbind2}-calling} version of \code{cbind} (into the
  \code{base} name space) and the same for \code{rbind}.\cr
  \code{methods:::bind_activation(FALSE)} reverts to the
  previous internal version of \code{cbind} which does not build on
  \code{cbind2}, see the examples.
}
\usage{
cbind2(x, y)
rbind2(x, y)
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
  A matrix (or matrix like object) combining the columns (or rows) of
  \code{x} and \code{y}.
}
\seealso{\code{\link{cbind}}, \code{\link{rbind}}.
}
\examples{
cbind2(1:3, 4)
m <- matrix(3:8, 2,3, dimnames=list(c("a","b"), LETTERS[1:3]))
cbind2(1:2, m) # keeps dimnames from m

### Note: Use the following activation if you want cbind() to work
### ----  on S4 objects -- be careful otherwise!

methods:::bind_activation(on = TRUE)
trace("cbind2")
cbind(a=1:3)# no call to cbind2()
cbind(a=1:3, four=4, 7:9)# calling cbind2() twice
untrace("cbind2")

\dontshow{
cbind(m,m+1,m+2)
cbind(m,a=1, ch=c("D","E"))
cbind(1,a=1:3, m) # ok with a warning
cbind(A=1, B=3, m, C=4)
}

## The following fails currently,
## since cbind() works recursively from the tail:
try( cbind(m, a=1, b=3) )

## turn off the `special cbind()' :
methods:::bind_activation(FALSE)
}
\keyword{array}
\keyword{manip}
