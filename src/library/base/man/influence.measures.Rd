\name{influence.measures}
\title{Regression Diagnostics}
\usage{
influence.measures(lm.obj)

rstandard(lm.obj)
rstudent(lm.obj)
dfbetas(lm.obj)
dffits(lm.obj)
covratio(lm.obj)
cooks.distance(lm.obj)
hat(xmat)
}
\alias{influence.measures}
\alias{hat}
\alias{rstudent}
\alias{dfbetas}
\alias{dffits}
\alias{covratio}
\alias{cooks.distance}
\arguments{
  \item{lm.obj}{the results returned by \code{lm}.}
  \item{xmat}{the `X' or design matrix.}
}
\description{
  This suite of functions can be used to compute some of the regression
  diagnostics discussed in Belsley, Kuh and Welsch (1980), and in Cook
  and Weisberg (1982).

  The primary function is \code{influence.measures} which produces a
  class \code{"lminf"} object tabular display showing the DFBETAS for
  each model variable, DFFITS, covariance ratios, Cook's distances and
  the diagonal elements of the hat matrix.  Cases which are influential
  with respect to any of these measures are marked with an asterisk.

  The functions \code{rstudent}, \code{dfbetas}, \code{dffits},
  \code{covratio} and \code{cooks.distance} provide direct access to the
  corresponding diagnostic quantities.
}
\references{
  Belsley, D. A., E. Kuh and R. E. Welsch (1980).
  \emph{Regression Diagnostics.}
  New York: Wiley.

  Cook, R. D. and S. Weisberg (1982).
  \emph{Residuals and Influence in Regression.}
  London: Chapman and Hall.
}
\seealso{
  \code{\link{lm.influence}}.
}
\examples{
## Analysis of the life-cycle savings data
## given in Belsley, Kuh and Welsch.
data(savings)
lm.SR <- lm(sr ~ pop15 + pop75 + dpi + ddpi, data = savings)
summary(inflm.SR <- influence.measures(lm.SR))
inflm.SR
which(apply(inflm.SR$is.inf, 1, any)) # which observations `are' influential
dim(dfb <- dfbetas(lm.SR))            # the 1st columns of influence.measures
all(dfb == inflm.SR$infmat[, 1:5])
rstudent(lm.SR)
dffits(lm.SR)
covratio(lm.SR)
}
\keyword{regression}
