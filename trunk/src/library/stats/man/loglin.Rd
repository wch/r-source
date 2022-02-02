% File src/library/stats/man/loglin.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2018 R Core Team
% Distributed under GPL 2 or later

\name{loglin}
\alias{loglin}
\title{Fitting Log-Linear Models}
\usage{
loglin(table, margin, start = rep(1, length(table)), fit = FALSE,
       eps = 0.1, iter = 20, param = FALSE, print = TRUE)
}
\description{
  \code{loglin} is used to fit log-linear models to multidimensional
  contingency tables by Iterative Proportional Fitting.
}
\arguments{
  \item{table}{a contingency table to be fit, typically the output from
    \code{table}.}
  \item{margin}{a list of vectors with the marginal totals to be fit.

    (Hierarchical) log-linear models can be specified in terms of these
    marginal totals which give the \sQuote{maximal} factor subsets contained
    in the model.  For example, in a three-factor model,
    \code{list(c(1, 2), c(1, 3))} specifies a model which contains
    parameters for the grand mean, each factor, and the 1-2 and 1-3
    interactions, respectively (but no 2-3 or 1-2-3 interaction), i.e.,
    a model where factors 2 and 3 are independent conditional on factor
    1 (sometimes represented as \sQuote{[12][13]}).

    The names of factors (i.e., \code{names(dimnames(table))}) may be
    used rather than numeric indices.
  }
  \item{start}{a starting estimate for the fitted table.  This optional
    argument is important for incomplete tables with structural zeros
    in \code{table} which should be preserved in the fit.  In this
    case, the corresponding entries in \code{start} should be zero and
    the others can be taken as one.}
  \item{fit}{a logical indicating whether the fitted values should be
    returned.}
  \item{eps}{maximum deviation allowed between observed and fitted
    margins.}
  \item{iter}{maximum number of iterations.}
  \item{param}{a logical indicating whether the parameter values should
    be returned.}
  \item{print}{a logical.  If \code{TRUE}, the number of iterations and
    the final deviation are printed.}
}
\value{
  A list with the following components.
  \item{lrt}{the Likelihood Ratio Test statistic.}
  \item{pearson}{the Pearson test statistic (X-squared).}
  \item{df}{the degrees of freedom for the fitted model.  There is no
    adjustment for structural zeros.}
  \item{margin}{list of the margins that were fit.  Basically the same
    as the input \code{margin}, but with numbers replaced by names
    where possible.}
  \item{fit}{An array like \code{table} containing the fitted values.
    Only returned if \code{fit} is \code{TRUE}.}
  \item{param}{A list containing the estimated parameters of the
    model.  The \sQuote{standard} constraints of zero marginal sums
    (e.g., zero row and column sums for a two factor parameter) are
    employed.  Only returned if \code{param} is \code{TRUE}.}
}
\details{
  The Iterative Proportional Fitting algorithm as presented in
  Haberman (1972) is used for fitting the model.  At most \code{iter}
  iterations are performed, convergence is taken to occur when the
  maximum deviation between observed and fitted margins is less than
  \code{eps}.  All internal computations are done in double precision;
  there is no limit on the number of factors (the dimension of the
  table) in the model.

  Assuming that there are no structural zeros, both the Likelihood
  Ratio Test and Pearson test statistics have an asymptotic chi-squared
  distribution with \code{df} degrees of freedom.

  Note that the IPF steps are applied to the factors in the order given
  in \code{margin}.  Hence if the model is decomposable and the order
  given in \code{margin} is a running intersection property ordering
  then IPF will converge in one iteration.

  Package \CRANpkg{MASS} contains \code{loglm}, a front-end to
  \code{loglin} which allows the log-linear model to be specified and
  fitted in a formula-based manner similar to that of other fitting
  functions such as \code{lm} or \code{glm}.
}

\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988).
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.

  Haberman, S. J. (1972).
  Algorithm AS 51: Log-linear fit for contingency tables.
  \emph{Applied Statistics}, \bold{21}, 218--225.
  \doi{10.2307/2346506}.

  Agresti, A. (1990).
  \emph{Categorical data analysis}.
  New York: Wiley.
}
\author{Kurt Hornik}
\seealso{
  \code{\link{table}}.

  \code{\link[MASS]{loglm}} in package \CRANpkg{MASS} for a
  user-friendly wrapper.

  \code{\link{glm}} for another way to fit log-linear models.
}
\examples{
## Model of joint independence of sex from hair and eye color.
fm <- loglin(HairEyeColor, list(c(1, 2), c(1, 3), c(2, 3)))
fm
1 - pchisq(fm$lrt, fm$df)
## Model with no three-factor interactions fits well.
}
\keyword{category}
\keyword{models}
