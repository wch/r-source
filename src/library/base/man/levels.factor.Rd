\name{levels.factor}
\title{Factor Levels Assignment}
\usage{
levels(x) <- value
}
\alias{levels<-.factor}
\description{
\code{levels<-} provides a way to alter the levels attribute of factor.
\code{value} can be a vector of character strings with length equal to
the number of levels of \code{x}, or a named list specifying how to
rename the levels.
}
\seealso{
\code{\link{factor}}, \code{\link{levels}},
\code{\link{levels<-}}, \code{\link{nlevels}}.
}
\examples{
# assign individual levels
x <- gl(2, 4, 8)
levels(x)[1] <- "low"
levels(x)[2] <- "high"

# or as a group
y <- gl(2, 4, 8)
levels(y) <- c("low", "high")

# combine some levels
z <- gl(3, 2, 12)
levels(z) <- c("A", "B", "A")

# same, using a named list
z <- gl(3, 2, 12)
levels(z) <- list(A=c(1,3), B=2)
}
\keyword{category}
