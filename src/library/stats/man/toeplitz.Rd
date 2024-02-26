% File src/library/stats/man/toeplitz.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2022 R Core Team
% Distributed under GPL 2 or later

\name{toeplitz}
\title{Create Symmetric and Asymmetric \I{Toeplitz} Matrix}
\alias{toeplitz}
\alias{toeplitz2}
\usage{% >> ../R/diffinv.R
toeplitz (x, r = NULL, symmetric = is.null(r))
toeplitz2(x, nrow = length(x) +1 - ncol, ncol = length(x) +1 - nrow)
}
\arguments{
  \item{x}{for \code{toeplitz(x, *)}: the first column of the \I{Toeplitz} matrix;
    for \code{toeplitz2(x, *)} it is the upper-and-left border of the
    \I{Toeplitz} matrix, i.e., from top-right to bottom-left, such that
    \code{T[i,j] == x[i-j + ncol]}.}
  \item{r}{the first row of the target \I{Toeplitz} matrix; only needed in
    asymmetric cases.}
  \item{symmetric}{optional \code{\link{logical}} indicating if the
    matrix should be symmetric.}% hard-wired to TRUE up to 2022.
  \item{nrow, ncol}{the number of rows and columns; only one needs to be specified.}
}
\description{
  In its simplest use, \code{toeplitz()} forms a symmetric \I{Toeplitz} matrix
  given its first column (or row).  For the general case, asymmetric and
  non-square \I{Toeplitz} matrices are formed either by specifying the first
  column and row separately,
  \preformatted{T1 <- toeplitz(col, row)}
  or by
  \preformatted{T <- toeplitz2(x, nr, nc)}
  where only one of \code{(nr, nc)} needs to be specified.
  In the latter case, the simple equivalence \eqn{T_{i,j} = x_{i-j + n_c}}
  is fulfilled where \eqn{n_c =}\code{ncol(T)}.
}
\value{
  The \eqn{n \times m}{n x m} \I{Toeplitz} matrix \eqn{T}; for
  \describe{
    \item{\code{toeplitz()}: }{\code{dim(T)} is \code{(n,m)} and \code{m == length(x)} and
      \code{n == m} in the symmetric case or \code{n == length(r)} otherwise.}
    \item{\code{toeplitz2()}: }{\code{dim(T) == c(nrow, ncol)}.}
  }
}
\author{A. Trapletti and Martin Maechler (speedup and asymmetric extensions)
}
\examples{
x <- 1:5
toeplitz (x)

T. <- toeplitz (1:5, 11:13) # with a  *Warning* x[1] != r[1]
T2 <- toeplitz2(c(13:12, 1:5), 5, 3)# this is the same matrix:
stopifnot(identical(T., T2))

# Matrix of character (could also have logical, raw, complex ..) {also warning}:
noquote(toeplitz(letters[1:4], LETTERS[20:26]))

## A convolution/smoother weight matrix :
m <- 17
k <- length(wts <- c(76, 99, 60, 20, 1))
n <- m-k+1
## Convolution
W <- toeplitz2(c(rep(0, m-k), wts, rep(0, m-k)), ncol=n)
\dontdiff{\dontshow{hadM <- isNamespaceLoaded("Matrix")}
## "display" nicely :
if(requireNamespace("Matrix"))
   print(Matrix::Matrix(W))    else {
   colnames(W) <- paste0(",", if(n <= 9) 1:n else c(1:9, letters[seq_len(n-9)]))
   print(W)
}
\dontshow{if(!hadM) unloadNamespace("Matrix")}}% revert state
## scale W to have column sums 1:
W. <- W / sum(wts)
all.equal(rep(1, ncol(W.)), colSums(W.), check.attributes = FALSE)
## Visualize "mass-preserving" convolution
x <- 1:n; f <- function(x) exp(-((x - .4*n)/3)^2)
y <- f(x) + rep_len(3:-2, n)/10
## Smoothing convolution:
y.hat <- W. \%*\% y # y.hat := smoothed(y) ("mass preserving" -> longer than y)
stopifnot(length(y.hat) == m, m == n + (k-1))
plot(x,y, type="b", xlim=c(1,m)); curve(f(x), 1,n, col="gray", lty=2, add=TRUE)
lines(1:m, y.hat, col=2, lwd=3)
rbind(sum(y), sum(y.hat)) ## mass preserved

## And, yes, convolve(y, *) does the same when called appropriately:
all.equal(c(y.hat), convolve(y, rev(wts/sum(wts)), type="open"))
}
\keyword{ts}

