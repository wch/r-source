\name{split}
\title{Divide into Groups}
\alias{split}
\alias{split.default}
\alias{split.data.frame}
\alias{split<-}
\alias{split<-.default}
\alias{split<-.data.frame}
\alias{unsplit}
\description{
  \code{split} divides the data in the vector \code{x} into the groups
  defined by \code{f}.  The assignment forms replace values
  corresponding to such a division.  Unsplit reverses the effect of
  \code{split}.
}
\usage{
split(x, f, drop = FALSE)
split(x, f, drop = FALSE) <- value
unsplit(value, f, drop = FALSE)
}
\arguments{
  \item{x}{vector or data frame containing values to be divided into groups.}
  \item{f}{a \dQuote{factor} in the sense that \code{\link{as.factor}(f)}
    defines the grouping, or a list of such factors in which case their
    interaction is used for the grouping.}
  \item{drop}{logical indicating if levels that do not occur should be dropped
    (if \code{f} is a \code{factor} or a list).}
  \item{value}{a list of vectors or data frames compatible with a
    splitting of \code{x}. Recycling applies if the lengths do not match.}
}
\details{
  \code{split} and \code{split<-} are generic functions with default and
  \code{data.frame} methods.

  \code{f} is recycled as necessary and if the length of \code{x} is not
  a multiple of the length of \code{f} a warning is printed.
  \code{unsplit} works only with lists of vectors.  The data frame
  method can also be used to split a matrix into a list of matrices,
  and the assignment form likewise, provided they are invoked
  explicitly.

  Any missing values in \code{f} are dropped together with the
  corresponding values of \code{x}.
}
\value{
  The value returned from \code{split} is a list of vectors containing
  the values for the groups.  The components of the list are named by
  the \emph{used} factor levels given by \code{f}.  (If \code{f} is
  longer than \code{x} then some of the components will be of zero length.)

  The assignment forms return their right hand side. \code{unsplit}
  returns a vector for which \code{split(x, f)} equals \code{value}
}
\seealso{
  \code{\link{cut}}
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\examples{
require(stats)
n <- 10; nn <- 100
g <- factor(round(n * runif(n * nn)))
x <- rnorm(n * nn) + sqrt(as.numeric(g))
xg <- split(x, g)
boxplot(xg, col = "lavender", notch = TRUE, varwidth = TRUE)
sapply(xg, length)
sapply(xg, mean)

## Calculate z-scores by group

z <- unsplit(lapply(split(x, g), scale), g)
tapply(z, g, mean)

# or

z <- x
split(z, g) <- lapply(split(x, g), scale)
tapply(z, g, sd)

## Split a matrix into a list by columns
ma <- cbind(x = 1:10, y = (-4:5)^2)
split(ma, col(ma))

split(1:10, 1:2)
}
\keyword{category}
