\name{lsfit}
\title{Find the Least Squares Fit}
\usage{
lsfit(x, y, wt = NULL, intercept = TRUE, tolerance = 1e-07, yname = NULL)
}
\alias{lsfit}
\arguments{
\item{x}{a matrix whose rows correspond to cases and whose columns
  correspond to variables.}
\item{y}{the responses, possibly a matrix if you want to fit multiple
  left hand sides.}
\item{wt}{an optional vector of weights for performing weighted least squares.}
\item{intercept}{whether or not an intercept term should be used.}
\item{tolerance}{the tolerance to be used in the matrix decomposition.}
\item{yname}{names to be used for the response variables.}
}
\description{
The least squares estimate of \bold{\eqn{\beta}{b}} in the model
\deqn{\bold{Y} = \bold{X \beta} + \bold{\epsilon}}{y = X b + e}
is found.
}
\details{
If weights are specified then a weighted least squares is performed
with the weight given to the \emph{j}th case specified by the \emph{j}th
entry in \code{wt}.

If any observation has a missing value in any field, that observation
is removed before the analysis is carried out.
This can be quite inefficient if there is a lot of missing data.

The implementation is via a modification of the LINPACK subroutines
which allow for multiple left-hand sides.
}
\value{
A list with the following named components:
\item{coef}{the least squares estimates of the coefficients in
  the model (\bold{\eqn{\beta}{b}} as stated above).}
\item{residuals}{residuals from the fit.}
\item{intercept}{indicates whether an intercept was fitted.}
\item{qr}{the QR decomposition of the design matrix.}
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
\code{\link{lm}} which usually is preferable;
\code{\link{ls.print}}, \code{\link{ls.diag}}.
}
\examples{
\dontshow{example("lm", echo = FALSE)}
##-- Using the same data as the lm(.) example:
lsD9 <- lsfit(x = unclass(gl(2,10)), y = weight)
ls.print(lsD9)
}
\keyword{regression}
