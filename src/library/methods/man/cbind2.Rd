% File src/library/methods/man/cbind2.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2018 R Core Team
% Copyright 2001-2018 The R Foundation
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
  or rows (\code{rbind2}).  These are (S4) generic functions with default
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
  arguments.  The homogeneous case, where all objects derive from some S4
  class, can be handled via S4 dispatch on the \code{\dots} argument via
  an externally defined S4 \code{cbind} (\code{rbind}) generic.

  Since (for legacy reasons) S3 dispatch is attempted first, it is
  generally a good idea to additionally define an S3 method on
  \code{cbind} (\code{rbind}) for the S4 class.  The S3 method will be
  invoked when the arguments include objects of the S4 class, along with
  arguments of classes for which no S3 method exists.  Also, in case there
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
  \code{x} and \code{y}.  Note that methods must construct
  \code{\link{colnames}} and \code{\link{rownames}} from the
  corresponding column and row names of \code{x} and \code{y} (but not
  from deparsing argument names such as in \code{\link{cbind}(...,
    deparse.level = d)} for \eqn{d \ge 1}{d >= 1}).
}
\seealso{\code{\link{cbind}}, \code{\link{rbind}};
  further, \code{\link[Matrix]{cBind}}, \code{\link[Matrix]{rBind}} in
  the \CRANpkg{Matrix} package.
}
\examples{
cbind2(1:3, 4)
m <- matrix(3:8, 2,3, dimnames=list(c("a","b"), LETTERS[1:3]))
cbind2(1:2, m) # keeps dimnames from m

## rbind() and cbind() now make use of rbind2()/cbind2() methods
setClass("Num", contains="numeric")
setMethod("cbind2", c("Num", "missing"),
          function(x,y, ...) { cat("Num-miss--meth\n"); as.matrix(x)})
setMethod("cbind2", c("Num","ANY"), function(x,y, ...) {
    cat("Num-A.--method\n") ; cbind(getDataPart(x), y, ...) })
setMethod("cbind2", c("ANY","Num"), function(x,y, ...) {
    cat("A.-Num--method\n") ; cbind(x, getDataPart(y), ...) })

a <- new("Num", 1:3)
trace("cbind2")
cbind(a)
cbind(a, four=4, 7:9)# calling cbind2() twice

cbind(m,a, ch=c("D","E"), a*3)
cbind(1,a, m) # ok with a warning
untrace("cbind2")
}
\keyword{array}
\keyword{manip}
