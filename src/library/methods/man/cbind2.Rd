% File src/library/methods/man/cbind2.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2015 R Core Team
% Copyright 2001-2011 The R Foundation
% Distributed under GPL 2 or later

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
  Combine two matrix-like \R objects by columns (\code{cbind2})
  or rows (\code{rbind2}). These are (S4) generic functions with default
  methods.
}
\details{
  The main use of \code{cbind2} (\code{rbind2}) is to be called
  recursively by \code{\link{cbind}()} (\code{rbind()}) when both of
  these requirements are met:
  \itemize{
    \item{There is at least one argument that is an S4 object,
      and}
    \item{S3 dispatch fails (see the Dispatch section under \link{cbind}).}
  }
  The methods on \code{cbind2} and \code{rbind2} effectively define the
  type promotion policy when combining a heterogeneous set of
  arguments. The homogeneous case, where all objects derive from some S4
  class, can be handled via S4 dispatch on the \code{\dots} argument via
  an externally defined S4 \code{cbind} (\code{rbind}) generic.

  Since (for legacy reasons) S3 dispatch is attempted first, it is
  generally a good idea to additionally define an S3 method on
  \code{cbind} (\code{rbind}) for the S4 class. The S3 method will be
  invoked when the arguments include objects of the S4 class, along with
  parameters of types for which no S3 method exists. Also, in case there
  is an argument that selects a different S3 method (like the one for
  \code{data.frame}), this S3 method serves to introduce an ambiguity in
  dispatch that triggers the recursive fallback to \code{cbind2}
  (\code{rbind2}). Otherwise, the other S3 method would be called, which
  may not be appropriate.
}

\usage{
cbind2(x, y, ...)
rbind2(x, y, ...)
}
\arguments{
  \item{x}{any \R object, typically matrix-like.}
  \item{y}{any \R object, typically similar to \code{x}, or missing
    completely.}
  \item{\dots}{optional arguments for methods.}
}
\section{Methods}{
  \describe{
    \item{\code{signature(x = "ANY", y = "ANY")}}{the default method
      using \R's internal code.}
    \item{\code{signature(x = "ANY", y = "missing")}}{the default method
      for one argument using \R's internal code.}
  }
}
%% The R code is currently in ../R/MethodsListClass.R + ../R/cbind-rbind.R
\value{
  A matrix (or matrix like object) combining the columns (or rows) of
  \code{x} and \code{y}.
}
\seealso{\code{\link{cbind}}, \code{\link{rbind}};
  further, \code{\link[Matrix]{cBind}}, \code{\link[Matrix]{rBind}} in
  the \CRANpkg{Matrix} package.
}
\examples{
cbind2(1:3, 4)
m <- matrix(3:8, 2,3, dimnames=list(c("a","b"), LETTERS[1:3]))
cbind2(1:2, m) # keeps dimnames from m

\dontshow{
### Note: Use the following activation if you want cbind() to work
### ----  on S4 objects -- be careful otherwise!

methods:::bind_activation(on = TRUE)
trace("cbind2")
cbind(a=1:3)# no call to cbind2()
cbind(a=1:3, four=4, 7:9)# calling cbind2() twice
untrace("cbind2")

cbind(m,m+1,m+2)
cbind(m,a=1, ch=c("D","E"))
cbind(1,a=1:3, m) # ok with a warning
cbind(A=1, B=3, m, C=4)

cbind(m, a=1, b=3)

## turn off the `special cbind()' :
methods:::bind_activation(FALSE)
}
}
\keyword{array}
\keyword{manip}
