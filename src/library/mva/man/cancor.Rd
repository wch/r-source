\name{cancor}
\title{Canonical Correlations}
\usage{
cancor(x, y, xcenter=TRUE, ycenter=TRUE)
}
\alias{cancor}
\arguments{
\item{x}{numeric matrix (\eqn{n \times p_1}{n * p1}), containing the x
    coordinates.}
\item{y}{numeric matrix (\eqn{n \times p_2}{n * p2}) of y coordinates.}
\item{xcenter}{logical or numeric vector of length \eqn{p_1}{p1},
    describing any centering to be done on the x values before the
    analysis.  If \code{TRUE} (default), subtract the column means,
    If \code{FALSE}, do not adjust the columns.
    Otherwise, a vector of values to be subtracted from the columns.}
\item{ycenter}{analogous to \code{xcenter}, but for the y values.}
}
\description{
    Compute the canonical correlations between \code{x} and \code{y}.
    The canonical correlation analysis seeks
    linear combinations of the y variables which are
    well explained by linear combinations of the x variables.
}
\value{
A list containing the following components:
\item{cor}{correlations.}
\item{xcoef}{estimated coefficients for the x variables.}
\item{ycoef}{estimated coefficients for the y variables.}
\item{xcenter}{the values used to adjust the x variables.}
\item{ycenter}{the values used to adjust the x variables.}
}
\references{
Hotelling H. (1936).
``Relations between two sets of variables''.
\emph{Biometrika}, \bold{28}, 321-327.

Seber, G. A. F. (1984).
\emph{Multivariate Analysis}. New York: Wiley, p. 506f.
}
\seealso{
\code{\link{qr}}, \code{\link{svd}}.
}
\examples{
data(savings)
pop <- savings[, 2:3]
oec <- savings[,-(2:3)]
str(cancor(pop, oec))

x <- matrix(rnorm(150), 50, 3)
y <- matrix(rnorm(250), 50, 5)
str(cxy <- cancor(x, y))
all(abs(cor(x \%*\% cxy$xcoef,
            y \%*\% cxy$ycoef)[,1:3] - diag(cxy $ cor)) < 1e-15)
all(abs(cor(x \%*\% cxy$xcoef) - diag(3)) < 1e-15)
all(abs(cor(y \%*\% cxy$ycoef) - diag(5)) < 1e-15)
}
\keyword{multivariate}
