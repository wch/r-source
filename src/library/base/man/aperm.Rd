\name{aperm}
\alias{aperm}
\title{Array Transposition}
\description{
  Transpose an array by permuting its dimensions and optionally resizing
  it.
}
\usage{
aperm(a, perm, resize = TRUE)
}
\arguments{
  \item{a}{the array to be transposed.}
  \item{perm}{the subscript permutation vector, which must be a
    permutation of the integers \code{1:n}, where \code{n} is the
    number of dimensions of \code{a}.  The default is to reverse the
    order of the dimensions.}
  \item{resize}{a flag indicating whether the vector should be
    resized as well as having its elements reordered (default \code{TRUE}).}
}
\value{
  A transposed version of array \code{a}, with subscripts permuted as
  indicated by the array \code{perm}.  If \code{resize} is \code{TRUE},
  the array is reshaped as well as having its elements permuted, the
  \code{dimnames} are also permuted;
  if \code{FALSE} then the returned object has the same dimensions as
  \code{a}, and the dimnames are dropped.

  The function \code{t} provides a faster and more convenient way of
  transposing matrices.
}
\author{Jonathan Rougier, \email{J.C.Rougier@durham.ac.uk} did the
  faster C implementation.
}
\seealso{\code{\link{t}}, to transpose matrices.
}
\examples{
# interchange the first two subscripts on a 3-way array x
x  <- array(1:24, 2:4)
xt <- aperm(x, c(2,1,3))
stopifnot(t(xt[,,2]) == x[,,2],
          t(xt[,,3]) == x[,,3],
          t(xt[,,4]) == x[,,4])

%-----------
\testonly{
%-----------
# check the names

x <- array(1:24, c(4, 6))
nms <- list(happy=letters[1:4], sad=LETTERS[1:6])

dimnames(x) <- nms
tmp <- aperm(x, c(2, 1))
stopifnot(all.equal(dimnames(tmp), nms[c(2, 1)]))

dimnames(x) <- c(nms[1], list(NULL))
tmp <- aperm(x, c(2, 1))
stopifnot(all.equal(dimnames(tmp), c(list(NULL), nms[1])))

names(nms) <- c("happy", "sad")
dimnames(x) <- nms
tmp <- aperm(x, c(2, 1))
stopifnot(all.equal(names(dimnames(tmp)), names(nms[c(2, 1)])))

dimnames(x) <- c(nms[1], list(NULL))
tmp <- aperm(x, c(2, 1))
stopifnot(all.equal(names(dimnames(tmp)), c("", names(nms)[1])))

# check resize

stopifnot(all(dim(aperm(x, c(2, 1), FALSE))==dim(x)))
stopifnot(is.null(dimnames(aperm(x, c(2, 1), FALSE))))

# check the types

x <- array(1:24, c(4, 6))
stopifnot(all.equal(aperm(x, c(2, 1)), t(x)))
stopifnot(is.integer(aperm(x, c(2, 1))))

x <- x + 0.0
stopifnot(all.equal(aperm(x, c(2, 1)), t(x)))
stopifnot(is.double(aperm(x, c(2, 1))))

x <- x + 0.0i
stopifnot(all.equal(aperm(x, c(2, 1)), t(x)))

x[] <- LETTERS[1:24]
stopifnot(all.equal(aperm(x, c(2, 1)), t(x)))

x <- array(list("fred"), c(4, 6))
x[[3, 4]] <- 1:10
stopifnot(all.equal(aperm(x, c(2, 1)), t(x)))
}%end{testonly}
}
\keyword{array}
