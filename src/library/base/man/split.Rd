% File src/library/base/man/split.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2012 R Core Team
% Distributed under GPL 2 or later

\name{split}
\title{Divide into Groups and Reassemble}
\alias{split}
\alias{split.default}
\alias{split.data.frame}
\alias{split<-}
\alias{split<-.default}
\alias{split<-.data.frame}
\alias{unsplit}
\description{
  \code{split} divides the data in the vector \code{x} into the groups
  defined by \code{f}.  The replacement forms replace values
  corresponding to such a division.  \code{unsplit} reverses the effect of
  \code{split}.
}
\usage{
split(x, f, drop = FALSE, ...)
split(x, f, drop = FALSE, ...) <- value
unsplit(value, f, drop = FALSE)
}
\arguments{
  \item{x}{vector or data frame containing values to be divided into groups.}
  \item{f}{a \sQuote{factor} in the sense that \code{\link{as.factor}(f)}
    defines the grouping, or a list of such factors in which case their
    interaction is used for the grouping.}
  \item{drop}{logical indicating if levels that do not occur should be dropped
    (if \code{f} is a \code{factor} or a list).}
  \item{value}{a list of vectors or data frames compatible with a
    splitting of \code{x}. Recycling applies if the lengths do not match.}
  \item{\dots}{further potential arguments passed to methods.}
}
\details{
  \code{split} and \code{split<-} are generic functions with default and
  \code{data.frame} methods.  The data frame method can also be used to
  split a matrix into a list of matrices, and the replacement form
  likewise, provided they are invoked explicitly.

  \code{unsplit} works with lists of vectors or data frames (assumed to
  have compatible structure, as if created by \code{split}). It puts
  elements or rows back in the positions given by \code{f}. In the data
  frame case, row names are obtained by unsplitting the row name
  vectors from the elements of \code{value}.

  \code{f} is recycled as necessary and if the length of \code{x} is not
  a multiple of the length of \code{f} a warning is printed.

  Any missing values in \code{f} are dropped together with the
  corresponding values of \code{x}.

  The default method calls \code{\link{interaction}}.  If the levels of
  the factors contain \samp{.} they may not be split as expected, so
  the method has argument \code{sep} which is use to join the levels.
}
\value{
  The value returned from \code{split} is a list of vectors containing
  the values for the groups.  The components of the list are named by
  the levels of \code{f} (after converting to a factor, or if already a
  factor and \code{drop = TRUE}, dropping unused levels).

  The replacement forms return their right hand side.  \code{unsplit}
  returns a vector or data frame for which \code{split(x, f)} equals
  \code{value}

}
\seealso{
  \code{\link{cut}} to categorize numeric values.

  \code{\link{strsplit}} to split strings.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\examples{
require(stats); require(graphics)
n <- 10; nn <- 100
g <- factor(round(n * runif(n * nn)))
x <- rnorm(n * nn) + sqrt(as.numeric(g))
xg <- split(x, g)
boxplot(xg, col = "lavender", notch = TRUE, varwidth = TRUE)
sapply(xg, length)
sapply(xg, mean)

### Calculate 'z-scores' by group (standardize to mean zero, variance one)
z <- unsplit(lapply(split(x, g), scale), g)

# or

zz <- x
split(zz, g) <- lapply(split(x, g), scale)

# and check that the within-group std dev is indeed one
tapply(z, g, sd)
tapply(zz, g, sd)


### data frame variation

## Notice that assignment form is not used since a variable is being added

g <- airquality$Month
l <- split(airquality, g)
l <- lapply(l, transform, Oz.Z = scale(Ozone))
aq2 <- unsplit(l, g)
head(aq2)
with(aq2, tapply(Oz.Z,  Month, sd, na.rm = TRUE))


### Split a matrix into a list by columns
ma <- cbind(x = 1:10, y = (-4:5)^2)
split(ma, col(ma))

split(1:10, 1:2)
}
\keyword{category}
