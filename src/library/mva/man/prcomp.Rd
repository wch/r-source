\name{prcomp}
\alias{prcomp}
\alias{plot.prcomp}
\alias{print.prcomp}
\alias{summary.prcomp}
\alias{print.summary.prcomp}
\title{Principal Components Analysis}
\usage{
prcomp(x, retx = TRUE, center = TRUE, scale. = FALSE, tol = NULL)
}
\arguments{
  \item{x}{a matrix (or data frame) which provides the data for the
    principal components analysis.}
  \item{retx}{a logical value indicating whether the rotated variables
    should be returned.}
  \item{center}{a logical value indicating whether the variables
    should be shifted to be zero centered. Alternately, a vector of
    length equal the number of columns of \code{x} can be supplied.
    The value is passed to \code{scale}.}
  \item{scale}{a logical value indicating whether the variables should
    be scaled to have unit variance before the analysis takes
    place. The default is \code{FALSE} for consistency with S, but
    in general scaling is advisable. Alternately, a vector of length
    equal the number of columns of \code{x} can be supplied.  The
    value is passed to \code{scale}.}
  \item{tol}{a value indicating the magnitude below which components
    should be omitted. With the default null setting, no components
    are omitted.  Other settings for tol could be \code{tol = 0} or
    \code{tol = sqrt(.Machine$double.eps)}.} 
}
\description{
  Performs a principal components analysis on the given data matrix
  and returns the results as an object of class \code{prcomp}.}
\value{
  \code{prcomp} returns an list with class \code{"prcomp"}
  containing the following components:
  \item{sdev}{the standard deviation of the principal components
    (i.e., the eigenvalues of the cov matrix, though the calculation
    is actually done with the singular values of the data matrix).}
  \item{rotation}{the matrix of variable loadings (i.e., a matrix
    whose olumns contain the eigenvectors).  The function
    \code{princomp} returns this in the element \code{loadings}.}
  \item{x}{if \code{retx} is true the value of the rotated data (the
    data multiplied by the \code{rotation} matrix) is returned.}
}
\details{
  The calculation is done by a singular-value decomposition of the
  data matrix, not by using
  eigen on the covariance matrix.  This is generally the preferred
  method for numerical accuracy.  The \code{print} method for the these
  objects prints the results in a nice format and the \code{plot} method
  produces a scree plot.
}
\references{
    Mardia, K. V., J. T. Kent, J and M. Bibby (1979),
    \emph{Multivariate Analysis}, London: Academic Press.

    Venables, W. N. and B. D. Ripley (1997),
    \emph{Modern Applied Statistics with S-PLUS}, Springer-Verlag.
}
\seealso{
    \code{\link{princomp}}, \code{\link{cor}}, \code{\link{cov}},
    \code{\link{svd}}, \code{\link{eigen}}.
}
\examples{
## the variances of the variables in the
## USArrests data vary by orders of magnitude
data(USArrests)
prcomp(USArrests)
prcomp(USArrests, scale = TRUE)
plot(prcomp(USArrests))
summary(prcomp(USArrests))
}
