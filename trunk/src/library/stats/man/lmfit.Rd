% File src/library/stats/man/lmfit.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{lm.fit}
\title{Fitter Functions for Linear Models}
\usage{
lm.fit (x, y,    offset = NULL, method = "qr", tol = 1e-7,
       singular.ok = TRUE, \dots)

lm.wfit(x, y, w, offset = NULL, method = "qr", tol = 1e-7,
        singular.ok = TRUE, \dots)

.lm.fit(x, y, tol = 1e-7)
}
\alias{lm.fit}
\alias{lm.wfit}
\alias{.lm.fit}
\description{
  These are the basic computing engines called by \code{\link{lm}} used
  to fit linear models.  These should usually \emph{not} be used
  directly unless by experienced users.  \code{.lm.fit()} is bare bone
  wrapper to the innermost QR-based C code, on which
  \code{\link{glm.fit}} and \code{\link{lsfit}} are based as well, for
  even more experienced users.
}
\arguments{
  \item{x}{design matrix of dimension \code{n * p}.}
  \item{y}{vector of observations of length \code{n}, or a matrix with
    \code{n} rows.}
  \item{w}{vector of weights (length \code{n}) to be used in the fitting
    process for the \code{wfit} functions.  Weighted least squares is
    used with weights \code{w}, i.e., \code{sum(w * e^2)} is minimized.}
  \item{offset}{(numeric of length \code{n}).  This can be used to
    specify an \emph{a priori} known component to be included in the
    linear predictor during fitting.}

  \item{method}{currently, only \code{method = "qr"} is supported.}

  \item{tol}{tolerance for the \code{\link{qr}} decomposition.  Default
    is 1e-7.}

  \item{singular.ok}{logical. If \code{FALSE}, a singular model is an
    error.}

  \item{\dots}{currently disregarded.}
}
\value{
  a \code{\link{list}} with components (for \code{lm.fit} and \code{lm.wfit})
  \item{coefficients}{\code{p} vector}
  \item{residuals}{\code{n} vector or matrix}
  \item{fitted.values}{\code{n} vector or matrix}
  \item{effects}{\code{n} vector of orthogonal single-df
    effects.  The first \code{rank} of them correspond to non-aliased
    coefficients, and are named accordingly.}
  \item{weights}{\code{n} vector --- \emph{only} for the \code{*wfit*}
    functions.}
  \item{rank}{integer, giving the rank}
  \item{df.residual}{degrees of freedom of residuals}
  \item{qr}{the QR decomposition, see \code{\link{qr}}.}

  Fits without any columns or non-zero weights do not have the
  \code{effects} and \code{qr} components.

  \code{.lm.fit()} returns a subset of the above, the \code{qr} part
  unwrapped, plus a logical component \code{pivoted} indicating if the
  underlying QR algorithm did pivot.
}
\seealso{
  \code{\link{lm}} which you should use for linear least squares regression,
  unless you know better.
}
\examples{
require(utils)
%% FIXME: Do something more sensible (non-random data) !!
set.seed(129)

n <- 7 ; p <- 2
X <- matrix(rnorm(n * p), n, p) # no intercept!
y <- rnorm(n)
w <- rnorm(n)^2

str(lmw <- lm.wfit(x = X, y = y, w = w))

str(lm. <- lm.fit (x = X, y = y))
\dontshow{
  ## These are the same calculations at C level, but a parallel BLAS
  ## might not do them the same way twice, and if seems serial MKL does not.
  lm.. <- .lm.fit(X,y)
  lm.w <- .lm.fit(X*sqrt(w), y*sqrt(w))
  id <- function(x, y) all.equal(x, y, tolerance = 1e-15, scale = 1)
  stopifnot(id(unname(lm.$coef), lm..$coef),
	    id(unname(lmw$coef), lm.w$coef))
}
\donttest{
if(require("microbenchmark")) {
  mb <- microbenchmark(lm(y~X), lm.fit(X,y), .lm.fit(X,y))
  print(mb)
  boxplot(mb, notch=TRUE)
}
}
%% do an example which sets 'tol' and gives a difference!
}
\keyword{regression}
\keyword{array}
