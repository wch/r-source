\name{levels}
\alias{levels}
\alias{levels<-}
\alias{levels<-.default}
\alias{levels<-.factor}
\title{Levels Attributes}
\usage{
levels(x)
levels(x) <- value
}
\description{
  \code{levels} provides access to the levels attribute of a variable.
  The first form returns the value of the levels of its argument
  and the second sets the attribute.

  The assignment form (\code{"levels<-"}) of \code{levels} is a generic
  function and new methods can be written for it.
  The most important method is that for \code{\link{factor}}s:
}
\arguments{
  \item{x}{an object, for example a factor.}
  \item{value}{A valid value for \code{levels(x)}.
    For the default method, \code{NULL} or a character vector.  For the
    \code{factor} method, a vector of character strings with length at
    least the number of levels of \code{x}, or a named list specifying how to
    rename the levels.}
}
\details{
  For the factor assignment method, a \code{NA} in \code{value}
  causes that level to be removed from the levels and the elements
  formerly with that level to be replaced by \code{NA}.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{nlevels}}.
}
\examples{
## assign individual levels
x <- gl(2, 4, 8)
levels(x)[1] <- "low"
levels(x)[2] <- "high"
x

## or as a group
y <- gl(2, 4, 8)
levels(y) <- c("low", "high")
y

## combine some levels
z <- gl(3, 2, 12)
levels(z) <- c("A", "B", "A")
z

## same, using a named list
z <- gl(3, 2, 12)
levels(z) <- list(A=c(1,3), B=2)
z

## we can add levels this way:
f <- factor(c("a","b"))
levels(f) <- c("c", "a", "b")
f

f <- factor(c("a","b"))
levels(f) <- list(C="C", A="a", B="b")
f
}
\keyword{category}
