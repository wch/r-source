\name{length}
\alias{length}
\alias{length<-}
\alias{length<-.factor}
\title{Length of an Object}
\description{
  Get or set the length of vectors (including lists) and factors, and of
  any other \R object for which a method has been defined.
}
\usage{
length(x)
length(x) <- value
}
\arguments{
  \item{x}{a vector or list or factor.}
  \item{value}{an integer.}
}
\value{
  The length of \code{x} as an \code{\link{integer}} of length 1,
  if \code{x} is (or can be coerced to) a vector or list.  Otherwise,
  \code{length} returns \code{NA}.
}
\details{
  Both functions are generic: you can write methods to handle specific
  classes of objects, see \link{InternalMethods}.

  The replacement form can be used to reset the length of a vector.  If
  a vector is shortened, extra values are discarded and when a vector is
  lengthened, it is padded out to its new length with \code{\link{NA}}s.
}
\seealso{\code{nchar} for counting the number of characters in
  character vectors.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\examples{
length(diag(4))# = 16 (4 x 4)
length(options())# 12 or more
length(y ~ x1 + x2 + x3)# 3
length(expression(x, {y <- x^2; y+2}, x^y)) # 3
}
\keyword{attribute}

