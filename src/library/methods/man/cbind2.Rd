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
}
\details{
  The main use of \code{cbind2} is to be called by \code{\link{cbind}()}
  \bold{if} that is activated.  This allows \code{cbind} to
  \dQuote{work} for formally classed (aka \sQuote{S4}) objects by
  providing S4 methods for these objects.  Currently, a call\cr
  \code{methods:::cbind_activation(TRUE)}\cr
  is needed to install a
  \dQuote{\code{cbind2}-calling} version of \code{cbind} (into the
  \code{base} namespace).
  To revert, \code{methods:::cbind_activation(FALSE)} reverts to the
  previous internal version of \code{cbind} which does not build on
  \code{cbind2}.
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
  \code{y}.  Currently, the default methods give column names \code{x}
  and \code{y}, but these will probably be eliminated in the future.
}
\seealso{\code{\link{cbind}}}
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
