% file add1.Rd
% copyright (C) 1998 B. D. Ripley
%
\name{add1}
\title{Add or Drop All Possible Single Terms to a Model}
\usage{
add1(object, scope, \dots)
add1.default(object, scope, scale = 0, test=c("none", "Chisq"),
             k = 2, trace = FALSE, \dots)
add1.lm(object, scope, scale = 0, test=c("none", "Chisq", "F"),
        x = NULL, k = 2, \dots)
add1.glm(object, scope, scale = 0, x = NULL, test=c("none", "Chisq"),
         k = 2, \dots)

drop1(object, scope, \dots)
drop1.default(object, scope, scale = 0, test=c("none", "Chisq"),
              k = 2, trace = FALSE, \dots)
drop1.lm(object, scope, scale = 0, all.cols = TRUE,
         test=c("none", "Chisq", "F"),k = 2, \dots)
drop1.glm(object, scope, scale = 0, test=c("none", "Chisq"),
          k = 2, \dots)
}
\alias{add1}
\alias{add1.default}
\alias{add1.lm}
\alias{add1.glm}
\alias{add1.mlm}
\alias{drop1}
\alias{drop1.default}
\alias{drop1.lm}
\alias{drop1.glm}
\alias{drop1.mlm}
\arguments{
 \item{object}{a fitted models object.}
 \item{scope}{a formula giving the terms to be considered for adding or
   dropping.}
 \item{scale}{an estimate of the residual mean square to be used in
   computing Cp. Ignored if \code{0} or \code{NULL}.}
 \item{test}{should the results include a test statistic relative to the
   original model?  The F test is only appropriate for \code{\link{lm}} and
   \code{\link{aov}} models. The \eqn{\chi^2}{Chisq} test can be an exact test
   (\code{lm} models with known scale) or a likelihood-ratio test depending
   on the method.}
 \item{k}{the penalty constant in AIC / Cp.}
 \item{trace}{if \code{TRUE}, print out progress reports.}
 \item{x}{a model matrix containing columns for the fitted model and all
   terms in the upper scope.  Useful if \code{add1} is to be called
   repeatedly.}
 \item{all.cols}{(Provided for compatibility with S.) Logical to specify
   whether all columns of the design matrix should be used. If
   \code{FALSE} then non-estimable columns are dropped, but the result
   is not usually statistically meaningful.}
}
\description{
 Compute all the single terms in the \code{scope} argument that can be
 added to or dropped from the model, fit those models and compute a
 table of the changes in fit.
}
\details{
  For \code{drop} methods, a missing \code{scope} is taken to be all
  terms in the model. The hierarchy is respected when considering terms
  to be added or dropped: all main effects contained in a second-order
  interaction must remain, and so on.

  The methods for \code{\link{lm}} and \code{\link{glm}} are more
  efficient in that they do not recompute the model matrix and call the
  \code{fit} methods directly.

  The default output table gives AIC, defined as minus twice log
  likelihood plus \eqn{2p} where \eqn{p} is the rank of the model (the
  number of effective parameters).  This is only defined up to an
  additive constant (like log-likelhoods).  For linear Gaussian models
  with fixed scale, the constant is chosen to give Mallows' Cp,
  \eqn{RSS/scale + 2p - n}.  Where Cp is used, the column is labelled as
  Cp rather than AIC.
}
\value{
  An object of class \code{"anova"} summarizing the differences in fit
  between the models.
}
\author{B.D. Ripley}
\note{These are not fully equivalent to the functions in S.  There is no
    \code{keep} argument, and the methods used are not quite so
    computationally efficient.
    
    Their authors' definitions of Mallows' Cp and Akaike's AIC are used, not
    those of the authors of the models chapter of S.
}
\seealso{\code{\link{step}}, \code{\link{aov}}, \code{\link{lm}},
    \code{\link{extractAIC}}.}
\examples{
example(step)#-> swiss
(alm1 <- add1(lm1, ~ I(Education^2) + .^2))
}
\keyword{models}
