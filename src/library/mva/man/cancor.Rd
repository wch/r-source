\name{cancor}
\title{Canonical Correlations}
\usage{
cancor(x, y, xcenter=TRUE, ycenter=TRUE)
}
\arguments{
\item{x}{a matrix containing the x coordinates.}
\item{y}{a matrix containing the y coordinates.}
\item{xcenter}{a parameter describing any centering to be done
on the x values before the analysis.  The default is substract
the column means, but it is possible to supply a vector of
values to be subtracted from the columns. It is possible to
turn off any adjustment.}
\item{ycenter}{like the parameter \code{xcenter}, but for the
y values.}
}
\description{
This function performs a canonical correlation
analysis for the given data.  The analysis seeks
linear combinations of the y variables which are
well explained by linear combinations of the
x variables.
}
\value{
A list containing the following components:
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
\emph{Multivariate Analysis}. New York: Wiley.
}
\seealso{
\code{\link{qr}}, \code{\link{svd}}.
}
\examples{
soil <- evap.x[,1:3]
air <- evap.x[,-(1:3)]
airsoil <- cancor(air, soil)
}
