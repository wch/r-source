% File src/library/base/man/levels.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2015 R Core Team
% Distributed under GPL 2 or later

\name{levels}
\alias{levels}
\alias{levels.default}
\alias{levels<-}
\alias{levels<-.factor}
\title{Levels Attributes}
\description{
  \code{levels} provides access to the levels attribute of a variable.
  The first form returns the value of the levels of its argument
  and the second sets the attribute.
}
\usage{
levels(x)
levels(x) <- value
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
  Both the extractor and replacement forms are generic and new methods
  can be written for them.  The most important method for the replacement
  function is that for \code{\link{factor}}s.

  For the factor replacement method, a \code{NA} in \code{value}
  causes that level to be removed from the levels and the elements
  formerly with that level to be replaced by \code{NA}.

  Note that for a factor, replacing the levels via
  \code{levels(x) <- value} is not the same as (and is preferred to)
  \code{attr(x, "levels") <- value}.

  The replacement function is \link{primitive}.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{nlevels}}, \code{\link{relevel}}, \code{\link{reorder}}.
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
z <- gl(3, 2, 12, labels = c("apple", "salad", "orange"))
z
levels(z) <- c("fruit", "veg", "fruit")
z

## same, using a named list
z <- gl(3, 2, 12, labels = c("apple", "salad", "orange"))
z
levels(z) <- list("fruit" = c("apple","orange"),
                  "veg"   = "salad")
z

## we can add levels this way:
f <- factor(c("a","b"))
levels(f) <- c("c", "a", "b")
f

f <- factor(c("a","b"))
levels(f) <- list(C = "C", A = "a", B = "b")
f
}
\keyword{category}
