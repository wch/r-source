\name{levels.factor}
\alias{levels<-.factor}
\title{Factor Levels Assignment}
\description{
  Provide a way to alter the levels attribute of factor.
}
\usage{
levels(x) <- value
}
\arguments{
  \item{x}{a factor.}
  \item{value}{a vector of character strings with length at least the
    number of levels of \code{x}, or a named list specifying how to
    rename the levels.}
}
\seealso{
  \code{\link{factor}},
  \code{\link{levels}},
  \code{\link{levels<-}},
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
