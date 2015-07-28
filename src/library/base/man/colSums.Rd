% File src/library/base/man/colSums.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2015 R Core Team
% Distributed under GPL 2 or later

\name{colSums}
\alias{colSums}
\alias{rowSums}
\alias{colMeans}
\alias{rowMeans}
\alias{.colSums}
\alias{.rowSums}
\alias{.colMeans}
\alias{.rowMeans}
\title{Form Row and Column Sums and Means}
\description{
  Form row and column sums and means for numeric arrays (or data frames).
}
\usage{
colSums (x, na.rm = FALSE, dims = 1)
rowSums (x, na.rm = FALSE, dims = 1)
colMeans(x, na.rm = FALSE, dims = 1)
rowMeans(x, na.rm = FALSE, dims = 1)

.colSums(x, m, n, na.rm = FALSE)
.rowSums(x, m, n, na.rm = FALSE)
.colMeans(x, m, n, na.rm = FALSE)
.rowMeans(x, m, n, na.rm = FALSE)
}
\arguments{
  \item{x}{an array of two or more dimensions, containing numeric,
    complex, integer or logical values, or a numeric data frame.  For
    \code{.colSums()} etc, a numeric, integer or logical matrix (or
    vector of length \code{m * n}).}
  \item{na.rm}{logical.  Should missing values (including \code{NaN})
    be omitted from the calculations?}
  \item{dims}{integer: Which dimensions are regarded as \sQuote{rows} or
    \sQuote{columns} to sum over.  For \code{row*}, the sum or mean is
    over dimensions \code{dims+1, \dots}; for \code{col*} it is over
    dimensions \code{1:dims}.}
  \item{m, n}{the dimensions of the matrix \code{x} for
    \code{.colSums()} etc.}
}
\details{
  These functions are equivalent to use of \code{\link{apply}} with
  \code{FUN = mean} or \code{FUN = sum} with appropriate margins, but
  are a lot faster.  As they are written for speed, they blur over some
  of the subtleties of \code{NaN} and \code{NA}.  If \code{na.rm =
    FALSE} and either \code{NaN} or \code{NA} appears in a sum, the
  result will be one of \code{NaN} or \code{NA}, but which might be
  platform-dependent.

  Notice that omission of missing values is done on a per-column or
  per-row basis, so column means may not be over the same set of rows,
  and vice versa.  To use only complete rows or columns, first select
  them with \code{\link{na.omit}} or \code{\link{complete.cases}}
  (possibly on the transpose of \code{x}).

  The versions with an initial dot in the name (\code{.colSums()} etc)
  are \sQuote{bare-bones} versions for use in programming: they apply
  only to numeric (like) matrices and do not name the result.
}
\value{
  A numeric or complex array of suitable size, or a vector if the result
  is one-dimensional.  For the first four functions the \code{dimnames}
  (or \code{names} for a vector result) are taken from the original
  array.

  If there are no values in a range to be summed over (after removing
  missing values with \code{na.rm = TRUE}), that
  component of the output is set to \code{0} (\code{*Sums}) or \code{NaN}
  (\code{*Means}), consistent with \code{\link{sum}} and
  \code{\link{mean}}.
}
\seealso{
  \code{\link{apply}}, \code{\link{rowsum}}
}
\examples{
## Compute row and column sums for a matrix:
x <- cbind(x1 = 3, x2 = c(4:1, 2:5))
rowSums(x); colSums(x)
dimnames(x)[[1]] <- letters[1:8]
rowSums(x); colSums(x); rowMeans(x); colMeans(x)
x[] <- as.integer(x)
rowSums(x); colSums(x)
x[] <- x < 3
rowSums(x); colSums(x)
x <- cbind(x1 = 3, x2 = c(4:1, 2:5))
x[3, ] <- NA; x[4, 2] <- NA
rowSums(x); colSums(x); rowMeans(x); colMeans(x)
rowSums(x, na.rm = TRUE); colSums(x, na.rm = TRUE)
rowMeans(x, na.rm = TRUE); colMeans(x, na.rm = TRUE)

## an array
dim(UCBAdmissions)
rowSums(UCBAdmissions); rowSums(UCBAdmissions, dims = 2)
colSums(UCBAdmissions); colSums(UCBAdmissions, dims = 2)

## complex case
x <- cbind(x1 = 3 + 2i, x2 = c(4:1, 2:5) - 5i)
x[3, ] <- NA; x[4, 2] <- NA
rowSums(x); colSums(x); rowMeans(x); colMeans(x)
rowSums(x, na.rm = TRUE); colSums(x, na.rm = TRUE)
rowMeans(x, na.rm = TRUE); colMeans(x, na.rm = TRUE)
}
\keyword{array}
\keyword{algebra}
\keyword{arith}
