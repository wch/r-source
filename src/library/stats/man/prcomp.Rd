\name{prcomp}
\alias{prcomp}
\alias{prcomp.formula}
\alias{prcomp.default}
\alias{plot.prcomp}
\alias{predict.prcomp}
\alias{print.prcomp}
\alias{summary.prcomp}
\alias{print.summary.prcomp}
\concept{PCA}
\title{Principal Components Analysis}
\description{
  Performs a principal components analysis on the given data matrix
  and returns the results as an object of class \code{prcomp}.}
\usage{
prcomp(x, \dots)

\method{prcomp}{formula}(formula, data = NULL, subset, na.action, \dots)

\method{prcomp}{default}(x, retx = TRUE, center = TRUE, scale. = FALSE, tol = NULL, \dots)

\method{predict}{prcomp}(object, newdata, \dots)
}
\arguments{
  \item{formula}{a formula with no response variable.}
  \item{data}{an optional data frame containing the variables in the
    formula \code{formula}.  By default the variables are taken from
    \code{environment(formula)}.}
  \item{subset}{an optional vector used to select rows (observations) of the
    data matrix \code{x}.}
  \item{na.action}{a function which indicates what should happen
    when the data contain \code{NA}s.  The default is set by
    the \code{na.action} setting of \code{\link{options}}, and is
    \code{\link{na.fail}} if that is unset. The \dQuote{factory-fresh}
    default is \code{\link{na.omit}}.}
  \item{\dots}{arguments passed to or from other methods. If \code{x} is
    a formula one might specify \code{scale.} or \code{tol}.}
  \item{x}{a numeric or complex matrix (or data frame) which provides
    the data for the principal components analysis.}
  \item{retx}{a logical value indicating whether the rotated variables
    should be returned.}
  \item{center}{a logical value indicating whether the variables
    should be shifted to be zero centered. Alternately, a vector of
    length equal the number of columns of \code{x} can be supplied.
    The value is passed to \code{scale}.}
  \item{scale.}{a logical value indicating whether the variables should
    be scaled to have unit variance before the analysis takes
    place. The default is \code{FALSE} for consistency with S, but
    in general scaling is advisable.  Alternatively, a vector of length
    equal the number of columns of \code{x} can be supplied.  The
    value is passed to \code{\link{scale}}.}
  \item{tol}{a value indicating the magnitude below which components
    should be omitted. (Components are omitted if their
    standard deviations are less than or equal to \code{tol} times the
    standard deviation of the first component.)
    With the default null setting, no components
    are omitted.  Other settings for tol could be \code{tol = 0} or
    \code{tol = sqrt(.Machine$double.eps)}, which would omit
    essentially constant components.}
  \item{object}{Object of class inheriting from \code{"prcomp"}}
  \item{newdata}{An optional data frame or matrix in which to look for
    variables with which to predict.  If omitted, the scores are used.
    If the original fit used a formula or a data frame or a matrix with
    column names, \code{newdata} must contain columns with the same
    names. Otherwise it must contain the same number of columns, to be
    used in the same order.
  }
}
\value{
  \code{prcomp} returns a list with class \code{"prcomp"}
  containing the following components:
  \item{sdev}{the standard deviations of the principal components
    (i.e., the square roots of the eigenvalues of the
    covariance/correlation matrix, though the calculation
    is actually done with the singular values of the data matrix).}
  \item{rotation}{the matrix of variable loadings (i.e., a matrix
    whose columns contain the eigenvectors).  The function
    \code{princomp} returns this in the element \code{loadings}.}
  \item{x}{if \code{retx} is true the value of the rotated data (the
    centred (and scaled if requested) data multiplied by the
    \code{rotation} matrix) is returned.  Hence, \code{cov(x)} is the
    diagonal matrix \code{diag(sdev^2)}.  For the formula method,
    \code{napredict} is applied to handle the treatment of values
    omitted by the \code{na.action}.}
  \item{center, scale}{the centering and scaling used, or \code{FALSE}.}
}
\details{
  The calculation is done by a singular value decomposition of the
  (centered and possibly scaled) data matrix, not by using
  \code{eigen} on the covariance matrix.  This
  is generally the preferred method for numerical accuracy.  The
  \code{print} method for the these objects prints the results in a nice
  format and the \code{plot} method produces a scree plot.
}
\note{
  The signs of the columns of the rotation matrix are arbitrary, and
  so may differ between different programs for PCA, and even between
  different builds of \R.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.

  Mardia, K. V., J. T. Kent, and J. M. Bibby (1979)
  \emph{Multivariate Analysis}, London: Academic Press.

  Venables, W. N. and B. D. Ripley (2002)
  \emph{Modern Applied Statistics with S}, Springer-Verlag.
}
\seealso{
  \code{\link{biplot.prcomp}},
  \code{\link{princomp}}, \code{\link{cor}}, \code{\link{cov}},
  \code{\link{svd}}, \code{\link{eigen}}.
}
\examples{
## the variances of the variables in the
## USArrests data vary by orders of magnitude, so scaling is appropriate
prcomp(USArrests)  # inappropriate
prcomp(USArrests, scale = TRUE)
prcomp(~ Murder + Assault + Rape, data = USArrests, scale = TRUE)
plot(prcomp(USArrests))
summary(prcomp(USArrests, scale = TRUE))
biplot(prcomp(USArrests, scale = TRUE))
}
\keyword{multivariate}
