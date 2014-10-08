% File src/library/base/man/which.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{which}
\alias{which}
\alias{arrayInd}
\title{Which indices are TRUE?}
\description{
  Give the \code{TRUE} indices of a logical object, allowing for array
  indices.
}
\usage{
which(x, arr.ind = FALSE, useNames = TRUE)
arrayInd(ind, .dim, .dimnames = NULL, useNames = FALSE)
}
\arguments{
 \item{x}{a \code{\link{logical}} vector or array.  \code{\link{NA}}s
   are allowed and omitted (treated as if \code{FALSE}).}
 \item{arr.ind}{logical; should \bold{arr}ay \bold{ind}ices be returned
   when \code{x} is an array?}

 \item{ind}{integer-valued index vector, as resulting from
   \code{which(x)}.}
 \item{.dim}{\code{\link{dim}(.)} integer vector}
 \item{.dimnames}{optional list of character \code{\link{dimnames}(.)}.
   If \code{useNames} is true, to be used for constructing dimnames for
   \code{arrayInd()} (and hence, \code{which(*, arr.ind=TRUE)}).
   If \code{\link{names}(.dimnames)} is not empty, these are used as
   column names.  \code{.dimnames[[1]]} is used as row names.}
 \item{useNames}{logical indicating if the value of \code{arrayInd()}
   should have (non-null) dimnames at all.}
}
\value{
  If \code{arr.ind == FALSE} (the default), an integer vector with
  \code{length} equal to \code{sum(x)}, i.e., to the number of
  \code{TRUE}s in \code{x}; Basically, the result is
  \code{(1:length(x))[x]}.

  If \code{arr.ind == TRUE} and \code{x} is an \code{\link{array}} (has
  a \code{\link{dim}} attribute), the result is
  \code{arrayInd(which(x), dim(x), dimnames(x))}, namely a matrix
  whose rows each are the indices of one element of \code{x}; see
  Examples below.
}
\note{
  Unlike most other base \R functions this does not coerce to \code{x}
  to logical: only arguemnts with \code{\link{typeof}} logical are
  accepted and others give an error.
}
\author{Werner Stahel and Peter Holzer (ETH Zurich) proposed the
  \code{arr.ind} option.}
\seealso{\code{\link{Logic}}, \code{\link{which.min}} for the index of
  the minimum or maximum, and \code{\link{match}} for the first index of
  an element in a vector, i.e., for a scalar \code{a}, \code{match(a, x)}
  is equivalent to  \code{min(which(x == a))} but much more efficient.}
\examples{
which(LETTERS == "R")
which(ll <- c(TRUE, FALSE, TRUE, NA, FALSE, FALSE, TRUE)) #> 1 3 7
names(ll) <- letters[seq(ll)]
which(ll)
which((1:12)\%\%2 == 0) # which are even?
which(1:10 > 3, arr.ind = TRUE)

( m <- matrix(1:12, 3, 4) )
div.3 <- m \%\% 3 == 0
which(div.3)
which(div.3, arr.ind = TRUE)
rownames(m) <- paste("Case", 1:3, sep = "_")
which(m \%\% 5 == 0, arr.ind = TRUE)

dim(m) <- c(2, 2, 3); m
which(div.3, arr.ind = FALSE)
which(div.3, arr.ind = TRUE)

vm <- c(m)
dim(vm) <- length(vm) #-- funny thing with  length(dim(...)) == 1
which(div.3, arr.ind = TRUE)
\dontshow{
dimnames(m) <- list(X = c("U", "V"), Z = c("y","z"), three = LETTERS[1:3])
wm <- which(m \%\% 3 == 0, arr.ind = TRUE)
vn <- vm; dimnames(vn) <- list(LETTERS[1:12])
wv <- which(vn \%\% 3 == 0, arr.ind = TRUE)

stopifnot(identical(wv, array(3L*(1:4), dim = c(4, 1),
                              dimnames = list(c("C", "F", "I", "L"), "dim1"))),
          identical(wm, array(c(1:2, 1:2, 2:1, 1:2, 1:3, 3L),
                              dim = 4:3,
                              dimnames = list(rep(c("U","V"),2),
                                              c("X", "Z", "three"))))
)
}%dont
}
\keyword{logic}
\keyword{attribute}
