% File src/library/stats/man/dist.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2010 R Core Team
% Distributed under GPL 2 or later

\name{dist}
\alias{dist}
\alias{print.dist}
\alias{format.dist}
\alias{labels.dist}
\alias{as.matrix.dist}
\alias{as.dist}
\alias{as.dist.default}
\concept{dissimilarity}
\title{Distance Matrix Computation}
\description{
  This function computes and returns the distance matrix computed by
  using the specified distance measure to compute the distances between
  the rows of a data matrix.
}
\usage{
dist(x, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)

as.dist(m, diag = FALSE, upper = FALSE)
\method{as.dist}{default}(m, diag = FALSE, upper = FALSE)

\method{print}{dist}(x, diag = NULL, upper = NULL,
      digits = getOption("digits"), justify = "none",
      right = TRUE, \dots)

\method{as.matrix}{dist}(x, \dots)
}
\arguments{
  \item{x}{a numeric matrix, data frame or \code{"dist"} object.}
  \item{method}{the distance measure to be used. This must be one of
    \code{"euclidean"}, \code{"maximum"}, \code{"manhattan"},
    \code{"canberra"}, \code{"binary"} or \code{"minkowski"}.
    Any unambiguous substring can be given.}
  \item{diag}{logical value indicating whether the diagonal of the
    distance matrix should be printed by \code{print.dist}.}
  \item{upper}{logical value indicating whether the upper triangle of the
    distance matrix should be printed by \code{print.dist}.}
  \item{p}{The power of the Minkowski distance.}
  \item{m}{An object with distance information to be converted to a
    \code{"dist"} object.  For the default method, a \code{"dist"}
    object, or a matrix (of distances) or an object which can be coerced
    to such a matrix using \code{\link{as.matrix}()}.  (Only the lower
    triangle of the matrix is used, the rest is ignored).}
  \item{digits, justify}{passed to \code{\link{format}} inside of
    \code{print()}.}
  \item{right, \dots}{further arguments, passed to other methods.}
}
\details{
  Available distance measures are (written for two vectors \eqn{x} and
  \eqn{y}):
  \describe{
    \item{\code{euclidean}:}{Usual distance between the two vectors (2
      norm aka \eqn{L_2}), \eqn{\sqrt{\sum_i (x_i - y_i)^2}}{sqrt(sum((x_i - y_i)^2))}.}

    \item{\code{maximum}:}{Maximum distance between two components of \eqn{x}
      and \eqn{y} (supremum norm)}

    \item{\code{manhattan}:}{Absolute distance between the two vectors (1 norm aka \eqn{L_1}).}

    \item{\code{canberra}:}{
      \eqn{\sum_i |x_i - y_i| / |x_i + y_i|}{sum(|x_i - y_i| / |x_i + y_i|)}.
      Terms with zero numerator and denominator are omitted from the sum
      and treated as if the values were missing.

      This is intended for non-negative values (e.g., counts): taking the
      absolute value of the denominator is a 1998 \R modification to
      avoid negative distances.
    }

    \item{\code{binary}:}{(aka \emph{asymmetric binary}): The vectors
      are regarded as binary bits, so non-zero elements are \sQuote{on}
      and zero elements are \sQuote{off}.  The distance is the
      \emph{proportion} of bits in which only one is on amongst those in
      which at least one is on.}

    \item{\code{minkowski}:}{The \eqn{p} norm, the \eqn{p}th root of the
      sum of the \eqn{p}th powers of the differences of the components.}
  }

  Missing values are allowed, and are excluded from all computations
  involving the rows within which they occur.
  Further, when \code{Inf} values are involved, all pairs of values are
  excluded when their contribution to the distance gave \code{NaN} or
  \code{NA}.
  If some columns are excluded in calculating a Euclidean, Manhattan,
  Canberra or Minkowski distance, the sum is scaled up proportionally to
  the number of columns used.  If all pairs are excluded when
  calculating a particular distance, the value is \code{NA}.

  The \code{"dist"} method of \code{as.matrix()} and \code{as.dist()}
  can be used for conversion between objects of class \code{"dist"}
  and conventional distance matrices.

  \code{as.dist()} is a generic function.  Its default method handles
  objects inheriting from class \code{"dist"}, or coercible to matrices
  using \code{\link{as.matrix}()}.  Support for classes representing
  distances (also known as dissimilarities) can be added by providing an
  \code{\link{as.matrix}()} or, more directly, an \code{as.dist} method
  for such a class.
}
\value{
  \code{dist} returns an object of class \code{"dist"}.

  The lower triangle of the distance matrix stored by columns in a
  vector, say \code{do}. If \code{n} is the number of
  observations, i.e., \code{n <- attr(do, "Size")}, then
  for \eqn{i < j \le n}, the dissimilarity between (row) i and j is
  \code{do[n*(i-1) - i*(i-1)/2 + j-i]}.
  The length of the vector is \eqn{n*(n-1)/2}, i.e., of order \eqn{n^2}.

  The object has the following attributes (besides \code{"class"} equal
  to \code{"dist"}):
  \item{Size}{integer, the number of observations in the dataset.}
  \item{Labels}{optionally, contains the labels, if any, of the
    observations of the dataset.}
  \item{Diag, Upper}{logicals corresponding to the arguments \code{diag}
    and \code{upper} above, specifying how the object should be printed.}
  \item{call}{optionally, the \code{\link{call}} used to create the
    object.}
  \item{method}{optionally, the distance method used; resulting from
    \code{\link{dist}()}, the (\code{\link{match.arg}()}ed) \code{method}
    argument.}
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.

  Mardia, K. V., Kent, J. T. and Bibby, J. M. (1979)
  \emph{Multivariate Analysis.} Academic Press.

  Borg, I. and Groenen, P. (1997)
  \emph{Modern Multidimensional Scaling.  Theory and Applications.}
  Springer.
}
\seealso{
  \code{\link[cluster]{daisy}} in the \CRANpkg{cluster} package with more
  possibilities in the case of \emph{mixed} (continuous / categorical)
  variables.
  \code{\link{hclust}}.
}
\examples{
require(graphics)

x <- matrix(rnorm(100), nrow = 5)
dist(x)
dist(x, diag = TRUE)
dist(x, upper = TRUE)
m <- as.matrix(dist(x))
d <- as.dist(m)
stopifnot(d == dist(x))

## Use correlations between variables "as distance"
dd <- as.dist((1 - cor(USJudgeRatings))/2)
round(1000 * dd) # (prints more nicely)
plot(hclust(dd)) # to see a dendrogram of clustered variables

## example of binary and canberra distances.
x <- c(0, 0, 1, 1, 1, 1)
y <- c(1, 0, 1, 1, 0, 1)
dist(rbind(x, y), method = "binary")
## answer 0.4 = 2/5
dist(rbind(x, y), method = "canberra")
## answer 2 * (6/5)

## To find the names
labels(eurodist)

## Examples involving "Inf" :
## 1)
x[6] <- Inf
(m2 <- rbind(x, y))
dist(m2, method = "binary")   # warning, answer 0.5 = 2/4
## These all give "Inf":
stopifnot(Inf == dist(m2, method =  "euclidean"),
          Inf == dist(m2, method =  "maximum"),
          Inf == dist(m2, method =  "manhattan"))
##  "Inf" is same as very large number:
x1 <- x; x1[6] <- 1e100
stopifnot(dist(cbind(x, y), method = "canberra") ==
    print(dist(cbind(x1, y), method = "canberra")))

## 2)
y[6] <- Inf #-> 6-th pair is excluded
dist(rbind(x, y), method = "binary"  )   # warning; 0.5
dist(rbind(x, y), method = "canberra"  ) # 3
dist(rbind(x, y), method = "maximum")    # 1
dist(rbind(x, y), method = "manhattan")  # 2.4
}
\keyword{multivariate}
\keyword{cluster}
