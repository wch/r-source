% File src/library/stats/man/lm.influence.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{lm.influence}
\title{Regression Diagnostics}
\usage{
influence(model, \dots)
\method{influence}{lm}(model, do.coef = TRUE, \dots)
\method{influence}{glm}(model, do.coef = TRUE, \dots)

lm.influence(model, do.coef = TRUE)
}
\alias{lm.influence}
\alias{influence}
\alias{influence.lm}
\alias{influence.glm}
\arguments{
  \item{model}{an object as returned by \code{\link{lm}} or \code{\link{glm}}.}
  \item{do.coef}{logical indicating if the changed \code{coefficients}
    (see below) are desired.  These need \eqn{O(n^2 p)} computing time.}
  \item{\dots}{further arguments passed to or from other methods.}
}
\description{
  This function provides the basic quantities which are
  used in forming a wide variety of diagnostics for
  checking the quality of regression fits.
}
\details{
  The \code{\link{influence.measures}()} and other functions listed in
  \bold{See Also} provide a more user oriented way of computing a
  variety of regression diagnostics.  These all build on
  \code{lm.influence}.  Note that for GLMs (other than the Gaussian
  family with identity link) these are based on one-step approximations
  which may be inadequate if a case has high influence.

  An attempt is made to ensure that computed hat values that are
  probably one are treated as one, and the corresponding rows in
  \code{sigma} and \code{coefficients} are \code{NaN}.  (Dropping such a
  case would normally result in a variable being dropped, so it is not
  possible to give simple drop-one diagnostics.)

  \code{\link{naresid}} is applied to the results and so will fill in
  with \code{NA}s it the fit had \code{na.action = na.exclude}.
}
\value{
  A list containing the following components of the same length or
  number of rows \eqn{n}, which is the number of non-zero weights.
  Cases omitted in the fit are omitted unless a \code{\link{na.action}}
  method was used (such as \code{\link{na.exclude}}) which restores them.

  \item{hat}{a vector containing the diagonal of the \sQuote{hat} matrix.}
  \item{coefficients}{(unless \code{do.coef} is false) a matrix whose
    i-th row contains the change in the estimated coefficients which
    results when the i-th case is dropped from the regression.  Note
    that aliased coefficients are not included in the matrix.}
  \item{sigma}{a vector whose i-th element contains the estimate
    of the residual standard deviation obtained when the i-th
    case is dropped from the regression.  (The approximations needed for
    GLMs can result in this being \code{NaN}.)}
  \item{wt.res}{a vector of \emph{weighted} (or for class \code{glm}
    rather \emph{deviance}) residuals.}
}
\note{
  The \code{coefficients} returned by the \R version
  of \code{lm.influence} differ from those computed by S.
  Rather than returning the coefficients which result
  from dropping each case, we return the changes in the coefficients.
  This is more directly useful in many diagnostic measures.\cr
  Since these need \eqn{O(n^2 p)} computing time, they can be omitted by
  \code{do.coef = FALSE}.

  Note that cases with \code{weights == 0} are \emph{dropped} (contrary
  to the situation in S).

  If a model has been fitted with \code{na.action = na.exclude} (see
  \code{\link{na.exclude}}), cases excluded in the fit \emph{are}
  considered here.
}
\references{
  See the list in the documentation for \code{\link{influence.measures}}.

  Chambers, J. M. (1992)
  \emph{Linear models.}
  Chapter 4 of \emph{Statistical Models in S}
  eds J. M. Chambers and T. J. Hastie, Wadsworth & Brooks/Cole.
}
\seealso{
\code{\link{summary.lm}} for \code{\link{summary}} and related methods;\cr
\code{\link{influence.measures}},\cr
\code{\link{hat}} for the hat matrix diagonals,\cr
\code{\link{dfbetas}},
\code{\link{dffits}},
\code{\link{covratio}},
\code{\link{cooks.distance}},
\code{\link{lm}}.
}
\examples{
## Analysis of the life-cycle savings data
## given in Belsley, Kuh and Welsch.
summary(lm.SR <- lm(sr ~ pop15 + pop75 + dpi + ddpi,
                    data = LifeCycleSavings),
        corr = TRUE)
utils::str(lmI <- lm.influence(lm.SR))

## For more "user level" examples, use example(influence.measures)
}
\keyword{regression}
