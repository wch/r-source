% File src/library/stats/man/princomp.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{princomp}
\alias{princomp}
\alias{princomp.formula}
\alias{princomp.default}
\alias{plot.princomp}
\alias{print.princomp}
\alias{predict.princomp}
\title{Principal Components Analysis}
\concept{PCA}
\usage{
princomp(x, \dots)

\method{princomp}{formula}(formula, data = NULL, subset, na.action, \dots)

\method{princomp}{default}(x, cor = FALSE, scores = TRUE, covmat = NULL,
         subset = rep_len(TRUE, nrow(as.matrix(x))), \dots)

\method{predict}{princomp}(object, newdata, \dots)
}
\arguments{
  \item{formula}{a formula with no response variable, referring only to
    numeric variables.}
  \item{data}{an optional data frame (or similar: see
    \code{\link{model.frame}}) containing the variables in the
    formula \code{formula}.  By default the variables are taken from
    \code{environment(formula)}.}
  \item{subset}{an optional vector used to select rows (observations) of the
    data matrix \code{x}.}
  \item{na.action}{a function which indicates what should happen
    when the data contain \code{NA}s.  The default is set by
    the \code{na.action} setting of \code{\link{options}}, and is
    \code{\link{na.fail}} if that is unset. The \sQuote{factory-fresh}
    default is \code{\link{na.omit}}.}
  \item{x}{a numeric matrix or data frame which provides the data for the
    principal components analysis.}
  \item{cor}{a logical value indicating whether the calculation should
    use the correlation matrix or the covariance matrix.  (The
    correlation matrix can only be used if there are no constant variables.)}
  \item{scores}{a logical value indicating whether the score on each
    principal component should be calculated.}
  \item{covmat}{a covariance matrix, or a covariance list as returned by
    \code{\link{cov.wt}} (and \code{\link[MASS:cov.rob]{cov.mve}} or
    \code{\link[MASS:cov.rob]{cov.mcd}} from package \CRANpkg{MASS}).
    If supplied, this is used rather than the covariance matrix of
    \code{x}.}
  \item{\dots}{arguments passed to or from other methods. If \code{x} is
    a formula one might specify \code{cor} or \code{scores}.}
  \item{object}{Object of class inheriting from \code{"princomp"}}
  \item{newdata}{An optional data frame or matrix in which to look for
    variables with which to predict.  If omitted, the scores are used.
    If the original fit used a formula or a data frame or a matrix with
    column names, \code{newdata} must contain columns with the same
    names. Otherwise it must contain the same number of columns, to be
    used in the same order.
  }
}
\description{
  \code{princomp} performs a principal components analysis on the given
  numeric data matrix and returns the results as an object of class
  \code{princomp}.
}
\value{
  \code{princomp} returns a list with class \code{"princomp"}
  containing the following components:
  \item{sdev}{the standard deviations of the principal components.}
  \item{loadings}{the matrix of variable loadings (i.e., a matrix
    whose columns contain the eigenvectors).  This is of class
    \code{"loadings"}: see \code{\link{loadings}} for its \code{print}
    method.}
  \item{center}{the means that were subtracted.}
  \item{scale}{the scalings applied to each variable.}
  \item{n.obs}{the number of observations.}
  \item{scores}{if \code{scores = TRUE}, the scores of the supplied
    data on the principal components.  These are non-null only if
    \code{x} was supplied, and if \code{covmat} was also supplied if it
    was a covariance list.  For the formula method,
    \code{\link{napredict}()} is applied to handle the treatment of
    values omitted by the \code{na.action}.}
  \item{call}{the matched call.}
  \item{na.action}{If relevant.}
}
\details{
  \code{princomp} is a generic function with \code{"formula"} and
  \code{"default"} methods.

  The calculation is done using \code{\link{eigen}} on the correlation or
  covariance matrix, as determined by \code{\link{cor}}.  This is done for
  compatibility with the S-PLUS result.  A preferred method of
  calculation is to use \code{\link{svd}} on \code{x}, as is done in
  \code{prcomp}.

  Note that the default calculation uses divisor \code{N} for the
  covariance matrix.

  The \code{\link{print}} method for these objects prints the
  results in a nice format and the \code{\link{plot}} method produces
  a scree plot (\code{\link{screeplot}}).  There is also a
  \code{\link{biplot}} method.

  If \code{x} is a formula then the standard NA-handling is applied to
  the scores (if requested): see \code{\link{napredict}}.

  \code{princomp} only handles so-called R-mode PCA, that is feature
  extraction of variables.  If a data matrix is supplied (possibly via a
  formula) it is required that there are at least as many units as
  variables.  For Q-mode PCA use \code{\link{prcomp}}.
}
\note{
  The signs of the columns of the loadings and scores are arbitrary, and
  so may differ between different programs for PCA, and even between
  different builds of \R.
}
\references{
  Mardia, K. V., J. T. Kent and J. M. Bibby (1979).
  \emph{Multivariate Analysis}, London: Academic Press.

  Venables, W. N. and B. D. Ripley (2002).
  \emph{Modern Applied Statistics with S}, Springer-Verlag.
}
\seealso{
  \code{\link{summary.princomp}}, \code{\link{screeplot}},
  \code{\link{biplot.princomp}},
  \code{\link{prcomp}}, \code{\link{cor}}, \code{\link{cov}},
  \code{\link{eigen}}.
}
\examples{
require(graphics)

## The variances of the variables in the
## USArrests data vary by orders of magnitude, so scaling is appropriate
(pc.cr <- princomp(USArrests))  # inappropriate
princomp(USArrests, cor = TRUE) # =^= prcomp(USArrests, scale=TRUE)
## Similar, but different:
## The standard deviations differ by a factor of sqrt(49/50)

summary(pc.cr <- princomp(USArrests, cor = TRUE))
loadings(pc.cr)  # note that blank entries are small but not zero
## The signs of the columns are arbitrary
plot(pc.cr) # shows a screeplot.
biplot(pc.cr)

## Formula interface
princomp(~ ., data = USArrests, cor = TRUE)

## NA-handling
USArrests[1, 2] <- NA
pc.cr <- princomp(~ Murder + Assault + UrbanPop,
                  data = USArrests, na.action = na.exclude, cor = TRUE)
\donttest{pc.cr$scores[1:5, ]}

## (Simple) Robust PCA:
## Classical:
(pc.cl  <- princomp(stackloss))
\donttest{## Robust:
(pc.rob <- princomp(stackloss, covmat = MASS::cov.rob(stackloss)))
}}
\keyword{multivariate}
