\name{length}
\title{Length of a Vector or List}
\usage{
length(x)
length(x) <- n
}
\alias{length}
\alias{length<-}
\arguments{
  \item{x}{a vector or list.}
  \item{n}{an integer.}
}
\description{
  Get or set the length of vectors (including lists).
}
\value{
  The length of \code{x} as an \code{\link{integer}} of length 1,
  if \code{x} is (or can be coerced to) a vector or list.  Otherwise,
  \code{length} returns \code{NA}.
}
\details{
  \code{length} is generic: you can write methods to handle
  of specific classes of objects, see \link{InternalMethods}.

  The replacement form can be used to reset
  the length of a vector.
  If a vector is shortened, extra values are discarded
  and when a vector is lengthened, it is padded out to
  its new length with \code{\link{NA}}s.
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

