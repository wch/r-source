\name{colSums}
\alias{colSums}
\alias{rowSums}
\alias{colMeans}
\alias{rowMeans}
\title{Form Row and Column Sums and Means}
\description{
  Form row and column sums and means for numeric arrays.
}
\usage{
colSums (x, na.rm = FALSE, dims = 1)
rowSums (x, na.rm = FALSE, dims = 1)
colMeans(x, na.rm = FALSE, dims = 1)
rowMeans(x, na.rm = FALSE, dims = 1)
}
\arguments{
  \item{x}{an array of two or more dimensions, containing numeric,
    complex, integer or logical values, or a numeric data frame.}
  \item{na.rm}{logical. Should missing values (including \code{NaN})
    be omitted from the calculations?}
  \item{dims}{Which dimensions are regarded as \dQuote{rows} or
    \dQuote{columns} to sum over.  For \code{row*}, the sum or mean is
    over dimensions \code{dims+1, \dots}; for \code{col*} it is over
    dimensions \code{1:dims}.}
}
\details{
  These functions are equivalent to use of \code{\link{apply}} with
  \code{FUN = mean} or \code{FUN = sum} with appropriate margins, but
  are a lot faster.  As they are written for speed, they blur over some
  of the subtleties of \code{NaN} and \code{NA}.  If \code{na.rm =
    FALSE} and either \code{NaN} or \code{NA} appears in a sum, the
  result will be one of \code{NaN} or \code{NA}, but which might be
  platform-dependent.
}
\value{
  A numeric or complex array of suitable size, or a vector if the result is
  one-dimensional.  The \code{dimnames} (or \code{names} for a vector
  result) are taken from the original array.

  If there are no values in a range to be summed over (after removing
  missing values with \code{na.rm = TRUE}), that
  component of the output is set to \code{0} (\code{*Sums}) or \code{NA}
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
