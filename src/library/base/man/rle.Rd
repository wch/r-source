\name{rle}
\title{Run Length Encoding}
\alias{rle}
\alias{inverse.rle}
\alias{print.rle}
\description{
  Compute the lengths and values of runs of equal values in a vector
  -- or the reverse operation.
}
\usage{
rle(x)
\method{print}{rle}(x, digits = getOption("digits"), \dots)
inverse.rle(x, \dots)
}
\arguments{
  \item{x}{a simple vector [as argument of rle()],\cr
    a class "rle" object [as argument of \code{print()} or
    \code{inverse.rle()}].}
  \item{digits, \dots}{potentially further arguments to the
    corresponding method.}
}
\value{
  \code{rle()} returns an object of class "rle" which is a list with
  components
  \item{lengths}{an integer vector containing the length of each run.}
  \item{values}{a vector of the same length as \code{lengths} with the
    corresponding values.}

  \code{inverse.rle()} is the inverse function of \code{rle()}.
}
\examples{
x <- rev(rep(6:10, 1:5))
rle(x)
## lengths [1:5]  5 4 3 2 1
## values  [1:5] 10 9 8 7 6

z <- c(TRUE,TRUE,FALSE,FALSE,TRUE,FALSE,TRUE,TRUE,TRUE)
rle(z)
rle(as.character(z))

stopifnot(x == inverse.rle(rle(x)),
          z == inverse.rle(rle(z)))
}
\keyword{manip}
