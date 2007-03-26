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
  \code{dimnames} are also permuted; if \code{resize = FALSE} then the
  returned object has the same dimensions as \code{a}, and the dimnames
  are dropped.  In each case other attributes are copied from \code{a}.

  The function \code{t} provides a faster and more convenient way of
  transposing matrices.
}
\author{Jonathan Rougier, \email{J.C.Rougier@durham.ac.uk} did the
  faster C implementation.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
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

}
\keyword{array}
