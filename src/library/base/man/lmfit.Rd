\name{lm.fit}
\title{Fitter Functions for Linear Models}
\usage{
lm.fit (x, y,    offset = NULL, method = "qr", tol = 1e-7, \dots)
lm.wfit(x, y, w, offset = NULL, method = "qr", tol = 1e-7, \dots)
lm.fit.null (x, y,    method = "qr", tol = 1e-7, \dots)
lm.wfit.null(x, y, w, method = "qr", tol = 1e-7, \dots)
}
\alias{lm.fit}
\alias{lm.wfit}
\alias{lm.fit.null}
\alias{lm.wfit.null}
\description{
  These are the basic computing engines called by \code{\link{lm}} used
  to fit linear models.  These should usually \emph{not} be used
  directly unless by experienced users.
}
\arguments{
  \item{x}{design matrix of dimension \code{n * p}.}
  \item{y}{vector of observations of length \code{n}.}
  \item{w}{vector of weights (length \code{n}) to be used in the fitting
    process for the \code{wfit} functions.  Weighted least squares is
    used with weights \code{w}, i.e., \code{sum(w * e^2)} is minimized.}
  \item{offset}{numeric of length \code{n}).  This can be used to
    specify an \emph{a priori} known component to be included in the
    linear predictor during fitting.}

  \item{method}{currently, only \code{method="qr"} is supported.}

  \item{tol}{tolerance for the \code{\link{qr}} decomposition.  Default
    is 1e-7.}

  \item{\dots}{currently disregarded.}
}
\details{
  The functions \code{lm.\{w\}fit.null} are called by \code{lm.fit} or
  \code{lm.wfit} respectively, when \code{x} has zero columns.
}
\value{
%% S(-PLUS) returns an object of class "lm"
%% such that print.lm, summary,... work; but that'd need more changes for R.
  a list with components
  \item{coefficients}{\code{p} vector}
  \item{residuals}{\code{n} vector}
  \item{fitted.values}{\code{n} vector}
  \item{effects}{\code{n} vector; \dots\dots}%% FIXME
  \item{weights}{\code{n} vector --- \emph{only} for the \code{*wfit*}
    functions.}
  \item{rank}{integer, giving the rank}
  \item{df.residual}{degrees of freedom of residuals}
%% these two *not* for the  lm*fit.null() functions:
  \item{qr}{the QR decomposition, see \code{\link{qr}}.}
}
\seealso{
  \code{\link{lm}} which you should use for linear least squares regression,
  unless you know better.
}
\examples{
%% FIXME: Do something more sensible (non-random data) !!
set.seed(129)
n <- 7 ; p <- 2
X <- matrix(rnorm(n * p), n,p) # no intercept!
y <- rnorm(n)
w <- rnorm(n)^2

str(lmw <- lm.wfit(x=X, y=y, w=w))

str(lm. <- lm.fit (x=X, y=y))

str(lm0 <-  lm.fit.null (x=X, y=y))
str(lmw0 <- lm.wfit.null(x=X, y=y,w=w))

%% do an example which sets `tol' and gives a difference!
}
\keyword{regression}
\keyword{array}
