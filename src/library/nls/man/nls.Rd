\name{nls}
\alias{nls}
\alias{anova.nls}
\alias{anovalist.nls}
\alias{coef.nls}
\alias{deviance.nls}
\alias{df.residual.nls}
\alias{fitted.nls}
\alias{logLik.nls}
\alias{print.nls}
\alias{residuals.nls}
\alias{summary.nls}
\alias{print.summary.nls}
\alias{weights.nls}
\title{Nonlinear Least Squares}
\usage{
nls(formula, data, start, control = nls.control(),
    algorithm = "default", trace = FALSE, subset, na.action)
}
%- maybe also `usage' for other objects documented here.
\arguments{
 \item{formula}{a nonlinear model formula including variables and parameters}
 \item{data}{an optional data frame in which to evaluate the variables in
     \code{formula}}
 \item{start}{a named list or named numeric vector of starting estimates}
 \item{control}{an optional list of control settings.  See
     \code{nlsControl} for the names of the settable control values and
     their effect.}
 \item{algorithm}{character string specifying the algorithm to use.
     The default algorithm is a Gauss-Newton algorithm. The other 
     alternative is "plinear", the Golub-Pereyra algorithm for
     partially linear least-squares models.}
 \item{subset}{an optional vector specifying a subset of observations
     to be used in the fitting process.}
\item{na.action}{a function which indicates what should happen
    when the data contain \code{NA}s.}
}
\description{
  Determine the nonlinear least squares estimates of the parameters.
}
\details{
  An \code{nls} object is a type of fitted model object.  It has methods
  for the generic functions \code{coef}, \code{formula}, \code{resid},
  \code{print}, \code{summary}, and \code{fitted}.
}
\value{
  A list of
  \item{m}{an \code{nlsModel} object incorporating the model}
  \item{data}{the expression that was passed to \code{nls} as the data
    argument.  The actual data values are present in the environment of
    the \code{m} component.}
}
\references{Bates, D.M. and Watts, D.G. (1988), \emph{Nonlinear Regression Analysis
    and Its Applications}, Wiley}
\author{Douglas M. Bates and Saikat DebRoy}
\seealso{\code{\link{nlsModel}}}
\examples{
library( nls )
data( DNase )
DNase1 <- DNase[ DNase$Run == 1, ]
## using a selfStart model
fm1DNase1 <- nls( density ~ SSlogis( log(conc), Asym, xmid, scal ), DNase1 )
summary( fm1DNase1 )
## using conditional linearity
fm2DNase1 <- nls( density ~ 1/(1 + exp(( xmid - log(conc) )/scal ) ),
                  data = DNase1,
                  start = list( xmid = 0, scal = 1 ),
                  alg = "plinear", trace = TRUE )
summary( fm2DNase1 )
## without conditional linearity
fm3DNase1 <- nls( density ~ Asym/(1 + exp(( xmid - log(conc) )/scal ) ),
                  data = DNase1,
                  start = list( Asym = 3, xmid = 0, scal = 1 ),
                  trace = TRUE )
summary( fm3DNase1 )
}
\keyword{nonlinear}
\keyword{regression}
\keyword{models}
