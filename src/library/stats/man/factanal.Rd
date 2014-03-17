% File src/library/stats/man/factanal.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2012 R Core Team
% Distributed under GPL 2 or later

\newcommand{\CRANpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}
\newcommand{\sspace}{\ifelse{latex}{\out{~}}{ }}

\name{factanal}
\alias{factanal}
\encoding{UTF-8}
\title{Factor Analysis}
\description{
  Perform maximum-likelihood factor analysis on a covariance matrix or
  data matrix.
}
\usage{
factanal(x, factors, data = NULL, covmat = NULL, n.obs = NA,
         subset, na.action, start = NULL,
         scores = c("none", "regression", "Bartlett"),
         rotation = "varimax", control = NULL, \dots)
}
\arguments{
  \item{x}{A formula or a numeric matrix or an object that can be
    coerced to a numeric matrix.}
  \item{factors}{The number of factors to be fitted.}
  \item{data}{An optional data frame (or similar: see
    \code{\link{model.frame}}), used only if \code{x} is a formula.  By
    default the variables are taken from \code{environment(formula)}.}
  \item{covmat}{A covariance matrix, or a covariance list as returned by
    \code{\link{cov.wt}}.  Of course, correlation matrices are covariance
    matrices.}
  \item{n.obs}{The number of observations, used if \code{covmat} is a
    covariance matrix.}
  \item{subset}{A specification of the cases to be used, if \code{x} is
    used as a matrix or formula.}
  \item{na.action}{The \code{na.action} to be used if \code{x} is
    used as a formula.}
  \item{start}{\code{NULL} or a matrix of starting values, each column
    giving an initial set of uniquenesses.}
  \item{scores}{Type of scores to produce, if any.  The default is none,
    \code{"regression"} gives Thompson's scores, \code{"Bartlett"} given
    Bartlett's weighted least-squares scores. Partial matching allows
    these names to be abbreviated.}
  \item{rotation}{character. \code{"none"} or the name of a function
    to be used to rotate the factors: it will be called with first
    argument the loadings matrix, and should return a list with component
    \code{loadings} giving the rotated loadings, or just the rotated loadings.}
  \item{control}{A list of control values,
    \describe{
      \item{nstart}{The number of starting values to be tried if
        \code{start = NULL}. Default 1.}
      \item{trace}{logical. Output tracing information? Default \code{FALSE}.}
      \item{lower}{The lower bound for uniquenesses during
        optimization. Should be > 0. Default 0.005.}
      \item{opt}{A list of control values to be passed to
        \code{\link{optim}}'s \code{control} argument.}
      \item{rotate}{a list of additional arguments for the rotation function.}
    }
  }
  \item{\dots}{Components of \code{control} can also be supplied as
    named arguments to \code{factanal}.}
}
\details{
  The factor analysis model is
  \deqn{x = \Lambda f + e}
  for a \eqn{p}--element row-vector \eqn{x}, a \eqn{p \times k}{p x k}
  matrix \eqn{\Lambda} of \emph{loadings}, a \eqn{k}--element vector
  \eqn{f} of \emph{scores} and a \eqn{p}--element vector \eqn{e}of
  errors.  None of the components other than \eqn{x} is observed, but
  the major restriction is that the scores be uncorrelated and of unit
  variance, and that the errors be independent with variances
  \eqn{\Psi}{Psi}, the \emph{uniquenesses}.  It is also common to
  scale the observed variables to unit variance, and done in this function.

  Thus factor analysis is in essence a model for the correlation matrix
  of \eqn{x},
  \deqn{\Sigma = \Lambda^\prime\Lambda + \Psi}{\Sigma = \Lambda'\Lambda + \Psi}
  There is still some indeterminacy in the model for it is unchanged
  if \eqn{\Lambda} is replaced by \eqn{G \Lambda} for
  any orthogonal matrix \eqn{G}.  Such matrices \eqn{G} are known as
  \emph{rotations} (although the term is applied also to non-orthogonal
  invertible matrices).

  If \code{covmat} is supplied it is used.  Otherwise \code{x} is used
  if it is a matrix, or a formula \code{x} is used with \code{data} to
  construct a model matrix, and that is used to construct a covariance
  matrix.  (It makes no sense for the formula to have a response, and
  all the variables must be numeric.)  Once a covariance matrix is found
  or calculated from \code{x}, it is converted to a correlation matrix
  for analysis.  The correlation matrix is returned as component
  \code{correlation} of the result.

  The fit is done by optimizing the log likelihood assuming multivariate
  normality over the uniquenesses.  (The maximizing loadings for given
  uniquenesses can be found analytically: Lawley & Maxwell (1971,
  p.\sspace{}27).)  All the starting values supplied in \code{start} are tried
  in turn and the best fit obtained is used.  If \code{start = NULL}
  then the first fit is started at the value suggested by
  \enc{Jöreskog}{Joreskog} (1963) and given by Lawley & Maxwell
  (1971, p.\sspace{}31), and then \code{control$nstart - 1} other values are
  tried, randomly selected as equal values of the uniquenesses.

  The uniquenesses are technically constrained to lie in \eqn{[0, 1]},
  but near-zero values are problematical, and the optimization is
  done with a lower bound of \code{control$lower}, default 0.005
  (Lawley & Maxwell, 1971, p.\sspace{}32).

  Scores can only be produced if a data matrix is supplied and used.
  The first method is the regression method of Thomson (1951), the
  second the weighted least squares method of Bartlett (1937, 8).
  Both are estimates of the unobserved scores \eqn{f}.  Thomson's method
  regresses (in the population) the unknown \eqn{f} on \eqn{x} to yield
  \deqn{\hat f = \Lambda^\prime \Sigma^{-1} x}{hat f = \Lambda' \Sigma^-1 x}
  and then substitutes the sample estimates of the quantities on the
  right-hand side.  Bartlett's method minimizes the sum of squares of
  standardized errors over the choice of \eqn{f}, given (the fitted)
  \eqn{\Lambda}.

  If \code{x} is a formula then the standard \code{NA}-handling is
  applied to the scores (if requested): see \code{\link{napredict}}.

  The \code{print} method (documented under \code{\link{loadings}})
  follows the factor analysis convention of drawing attention to the
  patterns of the results, so the default precision is three decimal
  places, and small loadings are suppressed.
}
\value{
  An object of class \code{"factanal"} with components
  \item{loadings}{A matrix of loadings, one column for each factor.  The
    factors are ordered in decreasing order of sums of squares of
    loadings, and given the sign that will make the sum of the loadings
    positive.  This is of class \code{"loadings"}: see
    \code{\link{loadings}} for its \code{print} method.}
  \item{uniquenesses}{The uniquenesses computed.}
  \item{correlation}{The correlation matrix used.}
  \item{criteria}{The results of the optimization: the value of the
    negative log-likelihood and information on the iterations used.}
  \item{factors}{The argument \code{factors}.}
  \item{dof}{The number of degrees of freedom of the factor analysis model.}
  \item{method}{The method: always \code{"mle"}.}
  \item{rotmat}{The rotation matrix if relevant.}
  \item{scores}{If requested, a matrix of scores.  \code{napredict} is
    applied to handle the treatment of values omitted by the \code{na.action}.}
  \item{n.obs}{The number of observations if available, or \code{NA}.}
  \item{call}{The matched call.}
  \item{na.action}{If relevant.}
  \item{STATISTIC, PVAL}{The significance-test statistic and P value, if
    it can be computed.}
}

\note{
  There are so many variations on factor analysis that it is hard to
  compare output from different programs.  Further, the optimization in
  maximum likelihood factor analysis is hard, and many other examples we
  compared had less good fits than produced by this function.  In
  particular, solutions which are \sQuote{Heywood cases} (with one or more
  uniquenesses essentially zero) are much more common than most texts
  and some other programs would lead one to believe.
}

\references{
  Bartlett, M. S. (1937) The statistical conception of mental factors.
  \emph{British Journal of Psychology}, \bold{28}, 97--104.

  Bartlett, M. S. (1938) Methods of estimating mental
  factors. \emph{Nature}, \bold{141}, 609--610.

  \enc{Jöreskog}{Joreskog}, K. G. (1963)
  \emph{Statistical Estimation in Factor Analysis.}  Almqvist and Wicksell.

  Lawley, D. N. and Maxwell, A. E. (1971) \emph{Factor Analysis as a
    Statistical Method.} Second edition. Butterworths.

  Thomson, G. H. (1951) \emph{The Factorial Analysis of Human Ability.}
  London University Press.
}

\seealso{
  \code{\link{loadings}} (which explains some details of the
  \code{print} method), \code{\link{varimax}}, \code{\link{princomp}},
  \code{\link{ability.cov}}, \code{\link{Harman23.cor}},
  \code{\link{Harman74.cor}}.

  Other rotation methods are available in various contributed packages,
  including \CRANpkg{GPArotation} and \CRANpkg{psych}.
}

\examples{
# A little demonstration, v2 is just v1 with noise,
# and same for v4 vs. v3 and v6 vs. v5
# Last four cases are there to add noise
# and introduce a positive manifold (g factor)
v1 <- c(1,1,1,1,1,1,1,1,1,1,3,3,3,3,3,4,5,6)
v2 <- c(1,2,1,1,1,1,2,1,2,1,3,4,3,3,3,4,6,5)
v3 <- c(3,3,3,3,3,1,1,1,1,1,1,1,1,1,1,5,4,6)
v4 <- c(3,3,4,3,3,1,1,2,1,1,1,1,2,1,1,5,6,4)
v5 <- c(1,1,1,1,1,3,3,3,3,3,1,1,1,1,1,6,4,5)
v6 <- c(1,1,1,2,1,3,3,3,4,3,1,1,1,2,1,6,5,4)
m1 <- cbind(v1,v2,v3,v4,v5,v6)
cor(m1)
factanal(m1, factors = 3) # varimax is the default
\donttest{factanal(m1, factors = 3, rotation = "promax")}
# The following shows the g factor as PC1
\donttest{prcomp(m1) # signs may depend on platform}

## formula interface
factanal(~v1+v2+v3+v4+v5+v6, factors = 3,
         scores = "Bartlett")$scores

\donttest{## a realistic example from Bartholomew (1987, pp. 61-65)
utils::example(ability.cov)
}}
\keyword{multivariate}
