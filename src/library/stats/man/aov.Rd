% File src/library/stats/man/aov.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\newcommand{\CRANpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}

\name{aov}
\alias{aov}
\alias{print.aov}
\alias{print.aovlist}
\alias{Error}
\title{Fit an Analysis of Variance Model}
\description{
  Fit an analysis of variance model by a call to \code{lm} for each stratum.
}
\usage{
aov(formula, data = NULL, projections = FALSE, qr = TRUE,
    contrasts = NULL, \dots)
}
\arguments{
  \item{formula}{A formula specifying the model.}
  \item{data}{A data frame in which the variables specified in the
    formula will be found. If missing, the variables are searched for in
    the standard way.}
  \item{projections}{Logical flag: should the projections be returned?}
  \item{qr}{Logical flag: should the QR decomposition be returned?}
  \item{contrasts}{A list of contrasts to be used for some of the factors
    in the formula. These are not used for any \code{Error} term, and
    supplying contrasts for factors only in the \code{Error} term will give
    a warning.}
  \item{\dots}{Arguments to be passed to \code{lm}, such as \code{subset}
    or \code{na.action}.  See \sQuote{Details} about \code{weights}.}
}
\details{
  This provides a wrapper to \code{lm} for fitting linear models to
  balanced or unbalanced experimental designs.

  The main difference from \code{lm} is in the way \code{print},
  \code{summary} and so on handle the fit: this is expressed in the
  traditional language of the analysis of variance rather than that of
  linear models.

  If the formula contains a single \code{Error} term, this is used to
  specify error strata, and appropriate models are fitted within each
  error stratum.

  The formula can specify multiple responses.

  Weights can be specified by a \code{weights} argument, but should not
  be used with an \code{Error} term, and are incompletely supported
  (e.g., not by \code{\link{model.tables}}).
}
\note{
  \code{aov} is designed for balanced designs, and the results can be
  hard to interpret without balance: beware that missing values in the
  response(s) will likely lose the balance.  If there are two or more
  error strata, the methods used are statistically inefficient without
  balance, and it may be better to use \code{\link[nlme]{lme}} in
  package \CRANpkg{nlme}.

  Balance can be checked with the \code{\link{replications}} function.

  The default \sQuote{contrasts} in \R are not orthogonal contrasts, and
  \code{aov} and its helper functions will work better with such
  contrasts: see the examples for how to select these.
}
\value{
  An object of class \code{c("aov", "lm")} or for multiple responses
  of class \code{c("maov", "aov", "mlm", "lm")} or for multiple error
  strata of class \code{"aovlist"}.  There are
  \code{\link{print}} and \code{\link{summary}} methods available for these.
}
\references{
  Chambers, J. M., Freeny, A and Heiberger, R. M. (1992)
  \emph{Analysis of variance; designed experiments.}
  Chapter 5 of \emph{Statistical Models in S}
  eds J. M. Chambers and T. J. Hastie, Wadsworth & Brooks/Cole.
}
\author{
  The design was inspired by the S function of the same name described
  in Chambers \emph{et al.} (1992).
}
\seealso{
  \code{\link{lm}}, \code{\link{summary.aov}},
  \code{\link{replications}}, \code{\link{alias}},
  \code{\link{proj}}, \code{\link{model.tables}}, \code{\link{TukeyHSD}}
}
\examples{
## From Venables and Ripley (2002) p.165.

## Set orthogonal contrasts.
op <- options(contrasts = c("contr.helmert", "contr.poly"))
( npk.aov <- aov(yield ~ block + N*P*K, npk) )
\donttest{summary(npk.aov)}
coefficients(npk.aov)

## to show the effects of re-ordering terms contrast the two fits
aov(yield ~ block + N * P + K, npk)
aov(terms(yield ~ block + N * P + K, keep.order = TRUE), npk)


## as a test, not particularly sensible statistically
npk.aovE <- aov(yield ~  N*P*K + Error(block), npk)
npk.aovE
summary(npk.aovE)
options(op)  # reset to previous
}
\keyword{models}
\keyword{regression}
