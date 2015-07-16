% File src/library/stats/man/glm.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\newcommand{\CRANpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}
\newcommand{\sspace}{\ifelse{latex}{\out{~}}{ }}

\name{glm}
\title{Fitting Generalized Linear Models}
\alias{glm}
\alias{glm.fit}
\alias{weights.glm}
%\alias{print.glm}
\concept{regression}
\concept{logistic}
\concept{log-linear}
\concept{loglinear}
\description{
  \code{glm} is used to fit generalized linear models, specified by
  giving a symbolic description of the linear predictor and a
  description of the error distribution.
}
\usage{
glm(formula, family = gaussian, data, weights, subset,
    na.action, start = NULL, etastart, mustart, offset,
    control = list(\dots), model = TRUE, method = "glm.fit",
    x = FALSE, y = TRUE, contrasts = NULL, \dots)

glm.fit(x, y, weights = rep(1, nobs),
        start = NULL, etastart = NULL, mustart = NULL,
        offset = rep(0, nobs), family = gaussian(),
        control = list(), intercept = TRUE)

\method{weights}{glm}(object, type = c("prior", "working"), \dots)
}
\arguments{
  \item{formula}{an object of class \code{"\link{formula}"} (or one that
    can be coerced to that class): a symbolic description of the
    model to be fitted.  The details of model specification are given
    under \sQuote{Details}.}

  \item{family}{a description of the error distribution and link
    function to be used in the model. This can be a character string
    naming a family function, a family function or the result of a call
    to a family function.  (See \code{\link{family}} for details of
    family functions.)}

  \item{data}{an optional data frame, list or environment (or object
    coercible by \code{\link{as.data.frame}} to a data frame) containing
    the variables in the model.  If not found in \code{data}, the
    variables are taken from \code{environment(formula)},
    typically the environment from which \code{glm} is called.}

  \item{weights}{an optional vector of \sQuote{prior weights} to be used
    in the fitting process.  Should be \code{NULL} or a numeric vector.}

  \item{subset}{an optional vector specifying a subset of observations
    to be used in the fitting process.}

  \item{na.action}{a function which indicates what should happen
    when the data contain \code{NA}s.  The default is set by
    the \code{na.action} setting of \code{\link{options}}, and is
    \code{\link{na.fail}} if that is unset.  The \sQuote{factory-fresh}
    default is \code{\link{na.omit}}.  Another possible value is
    \code{NULL}, no action.  Value \code{\link{na.exclude}} can be useful.}

  \item{start}{starting values for the parameters in the linear predictor.}

  \item{etastart}{starting values for the linear predictor.}

  \item{mustart}{starting values for the vector of means.}

  \item{offset}{this can be used to specify an \emph{a priori} known
    component to be included in the linear predictor during fitting.
    This should be \code{NULL} or a numeric vector of length equal to
    the number of cases.  One or more \code{\link{offset}} terms can be
    included in the formula instead or as well, and if more than one is
    specified their sum is used.  See \code{\link{model.offset}}.}

  \item{control}{a list of parameters for controlling the fitting
    process.  For \code{glm.fit} this is passed to
    \code{\link{glm.control}}.}

  \item{model}{a logical value indicating whether \emph{model frame}
    should be included as a component of the returned value.}

  \item{method}{the method to be used in fitting the model.  The default
    method \code{"glm.fit"} uses iteratively reweighted least squares
    (IWLS): the alternative \code{"model.frame"} returns the model frame
    and does no fitting.

    User-supplied fitting functions can be supplied either as a function
    or a character string naming a function, with a function which takes
    the same arguments as \code{glm.fit}.  If specified as a character
    string it is looked up from within the \pkg{stats} namespace.
  }

  \item{x, y}{For \code{glm}:
    logical values indicating whether the response vector and model
    matrix used in the fitting process should be returned as components
    of the returned value.

    For \code{glm.fit}: \code{x} is a design matrix of dimension
    \code{n * p}, and \code{y} is a vector of observations of length
    \code{n}.
  }

  \item{contrasts}{an optional list. See the \code{contrasts.arg}
    of \code{model.matrix.default}.}

  \item{intercept}{logical. Should an intercept be included in the
    \emph{null} model?}

  \item{object}{an object inheriting from class \code{"glm"}.}
  \item{type}{character, partial matching allowed.  Type of weights to
    extract from the fitted model object.}

  \item{\dots}{
    For \code{glm}: arguments to be used to form the default
    \code{control} argument if it is not supplied directly.

    For \code{weights}: further arguments passed to or from other methods.
  }
}
\details{
  A typical predictor has the form \code{response ~ terms} where
  \code{response} is the (numeric) response vector and \code{terms} is a
  series of terms which specifies a linear predictor for
  \code{response}.  For \code{binomial} and \code{quasibinomial}
  families the response can also be specified as a \code{\link{factor}}
  (when the first level denotes failure and all others success) or as a
  two-column matrix with the columns giving the numbers of successes and
  failures.  A terms specification of the form \code{first + second}
  indicates all the terms in \code{first} together with all the terms in
  \code{second} with any duplicates removed.

  A specification of the form \code{first:second} indicates the the set
  of terms obtained by taking the interactions of all terms in
  \code{first} with all terms in \code{second}.  The specification
  \code{first*second} indicates the \emph{cross} of \code{first} and
  \code{second}.  This is the same as \code{first + second +
  first:second}.

  The terms in the formula will be re-ordered so that main effects come
  first, followed by the interactions, all second-order, all third-order
  and so on: to avoid this pass a \code{terms} object as the formula.

  Non-\code{NULL} \code{weights} can be used to indicate that different
  observations have different dispersions (with the values in
  \code{weights} being inversely proportional to the dispersions); or
  equivalently, when the elements of \code{weights} are positive
  integers \eqn{w_i}, that each response \eqn{y_i} is the mean of
  \eqn{w_i} unit-weight observations.  For a binomial GLM prior weights
  are used to give the number of trials when the response is the
  proportion of successes: they would rarely be used for a Poisson GLM.


  \code{glm.fit} is the workhorse function: it is not normally called
  directly but can be more efficient where the response vector and
  design matrix have already been calculated.

  If more than one of \code{etastart}, \code{start} and \code{mustart}
  is specified, the first in the list will be used.  It is often
  advisable to supply starting values for a \code{\link{quasi}} family,
  and also for families with unusual links such as \code{gaussian("log")}.

  All of \code{weights}, \code{subset}, \code{offset}, \code{etastart}
  and \code{mustart} are evaluated in the same way as variables in
  \code{formula}, that is first in \code{data} and then in the
  environment of \code{formula}.

  For the background to warning messages about \sQuote{fitted probabilities
    numerically 0 or 1 occurred} for binomial GLMs, see Venables &
  Ripley (2002, pp.\sspace{}197--8).
}

\section{Fitting functions}{
  The argument \code{method} serves two purposes.  One is to allow the
  model frame to be recreated with no fitting.  The other is to allow
  the default fitting function \code{glm.fit} to be replaced by a
  function which takes the same arguments and uses a different fitting
  algorithm.  If \code{glm.fit} is supplied as a character string it is
  used to search for a function of that name, starting in the
  \pkg{stats} namespace.

  The class of the object return by the fitter (if any) will be
  prepended to the class returned by \code{glm}.
}

\value{
  \code{glm} returns an object of class inheriting from \code{"glm"}
  which inherits from the class \code{"lm"}. See later in this section.
  If a non-standard \code{method} is used, the object will also inherit
  from the class (if any) returned by that function.

  The function \code{\link{summary}} (i.e., \code{\link{summary.glm}}) can
  be used to obtain or print a summary of the results and the function
  \code{\link{anova}} (i.e., \code{\link{anova.glm}})
  to produce an analysis of variance table.

  The generic accessor functions \code{\link{coefficients}},
  \code{effects}, \code{fitted.values} and \code{residuals} can be used to
  extract various useful features of the value returned by \code{glm}.

  \code{weights} extracts a vector of weights, one for each case in the
  fit (after subsetting and \code{na.action}).

  An object of class \code{"glm"} is a list containing at least the
  following components:

  \item{coefficients}{a named vector of coefficients}
  \item{residuals}{the \emph{working} residuals, that is the residuals
    in the final iteration of the IWLS fit.  Since cases with zero
    weights are omitted, their working residuals are \code{NA}.}
  \item{fitted.values}{the fitted mean values, obtained by transforming
    the linear predictors by the inverse of the link function.}
  \item{rank}{the numeric rank of the fitted linear model.}
  \item{family}{the \code{\link{family}} object used.}
  \item{linear.predictors}{the linear fit on link scale.}
  \item{deviance}{up to a constant, minus twice the maximized
    log-likelihood.  Where sensible, the constant is chosen so that a
    saturated model has deviance zero.}
  \item{aic}{A version of Akaike's \emph{An Information Criterion},
    minus twice the maximized log-likelihood plus twice the number of
    parameters, computed by the \code{aic} component of the family.
    For binomial and Poison families the dispersion is
    fixed at one and the number of parameters is the number of
    coefficients. For gaussian, Gamma and inverse gaussian families the
    dispersion is estimated from the residual deviance, and the number
    of parameters is the number of coefficients plus one.  For a
    gaussian family the MLE of the dispersion is used so this is a valid
    value of AIC, but for Gamma and inverse gaussian families it is not.
    For families fitted by quasi-likelihood the value is \code{NA}.}
  \item{null.deviance}{The deviance for the null model, comparable with
    \code{deviance}. The null model will include the offset, and an
    intercept if there is one in the model.  Note that this will be
    incorrect if the link function depends on the data other than
    through the fitted mean: specify a zero offset to force a correct
    calculation.}
  \item{iter}{the number of iterations of IWLS used.}
  \item{weights}{the \emph{working} weights, that is the weights
    in the final iteration of the IWLS fit.}
  \item{prior.weights}{the weights initially supplied, a vector of
    \code{1}s if none were.}
  \item{df.residual}{the residual degrees of freedom.}
  \item{df.null}{the residual degrees of freedom for the null model.}
  \item{y}{if requested (the default) the \code{y} vector
    used. (It is a vector even for a binomial model.)}
  \item{x}{if requested, the model matrix.}
  \item{model}{if requested (the default), the model frame.}
  \item{converged}{logical. Was the IWLS algorithm judged to have converged?}
  \item{boundary}{logical. Is the fitted value on the boundary of the
    attainable values?}
  \item{call}{the matched call.}
  \item{formula}{the formula supplied.}
  \item{terms}{the \code{\link{terms}} object used.}
  \item{data}{the \code{data argument}.}
  \item{offset}{the offset vector used.}
  \item{control}{the value of the \code{control} argument used.}
  \item{method}{the name of the fitter function used, currently always
    \code{"glm.fit"}.}
  \item{contrasts}{(where relevant) the contrasts used.}
  \item{xlevels}{(where relevant) a record of the levels of the factors
    used in fitting.}
  \item{na.action}{(where relevant) information returned by
    \code{\link{model.frame}} on the special handling of \code{NA}s.}

  In addition, non-empty fits will have components \code{qr}, \code{R}
  and \code{effects} relating to the final weighted linear fit.

  Objects of class \code{"glm"} are normally of class \code{c("glm",
    "lm")}, that is inherit from class \code{"lm"}, and well-designed
  methods for class \code{"lm"} will be applied to the weighted linear
  model at the final iteration of IWLS.  However, care is needed, as
  extractor functions for class \code{"glm"} such as
  \code{\link{residuals}} and \code{weights} do \bold{not} just pick out
  the component of the fit with the same name.

  If a \code{\link{binomial}} \code{glm} model was specified by giving a
  two-column response, the weights returned by \code{prior.weights} are
  the total numbers of cases (factored by the supplied case weights) and
  the component \code{y} of the result is the proportion of successes.
}
\seealso{
  \code{\link{anova.glm}}, \code{\link{summary.glm}}, etc. for
  \code{glm} methods,
  and the generic functions \code{\link{anova}}, \code{\link{summary}},
  \code{\link{effects}}, \code{\link{fitted.values}},
  and \code{\link{residuals}}.

  \code{\link{lm}} for non-generalized \emph{linear} models (which SAS
  calls GLMs, for \sQuote{general} linear models).

  \code{\link{loglin}} and \code{\link[MASS]{loglm}} (package
  \CRANpkg{MASS}) for fitting log-linear models (which binomial and
  Poisson GLMs are) to contingency tables.

  \code{bigglm} in package \CRANpkg{biglm} for an alternative
  way to fit GLMs to large datasets (especially those with many cases).

  \code{\link{esoph}}, \code{\link{infert}} and
  \code{\link{predict.glm}} have examples of fitting binomial glms.
}
\author{
  The original \R implementation of \code{glm} was written by Simon
  Davies working for Ross Ihaka at the University of Auckland, but has
  since been extensively re-written by members of the R Core team.

  The design was inspired by the S function of the same name described
  in Hastie & Pregibon (1992).
}
\references{
  Dobson, A. J. (1990)
  \emph{An Introduction to Generalized Linear Models.}
  London: Chapman and Hall.

  Hastie, T. J. and Pregibon, D. (1992)
  \emph{Generalized linear models.}
  Chapter 6 of \emph{Statistical Models in S}
  eds J. M. Chambers and T. J. Hastie, Wadsworth & Brooks/Cole.

  McCullagh P. and Nelder, J. A. (1989)
  \emph{Generalized Linear Models.}
  London: Chapman and Hall.

  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.}
  New York: Springer.
}

\examples{
## Dobson (1990) Page 93: Randomized Controlled Trial :
counts <- c(18,17,15,20,10,20,25,13,12)
outcome <- gl(3,1,9)
treatment <- gl(3,3)
print(d.AD <- data.frame(treatment, outcome, counts))
glm.D93 <- glm(counts ~ outcome + treatment, family = poisson())
anova(glm.D93)
\donttest{summary(glm.D93)}

\dontrun{## an example with offsets from Venables & Ripley (2002, p.189)
utils::data(anorexia, package = "MASS")

anorex.1 <- glm(Postwt ~ Prewt + Treat + offset(Prewt),
                family = gaussian, data = anorexia)
summary(anorex.1)
}

# A Gamma example, from McCullagh & Nelder (1989, pp. 300-2)
clotting <- data.frame(
    u = c(5,10,15,20,30,40,60,80,100),
    lot1 = c(118,58,42,35,27,25,21,19,18),
    lot2 = c(69,35,26,21,18,16,13,12,12))
summary(glm(lot1 ~ log(u), data = clotting, family = Gamma))
summary(glm(lot2 ~ log(u), data = clotting, family = Gamma))

\dontrun{
## for an example of the use of a terms object as a formula
demo(glm.vr)
}}
\keyword{models}
\keyword{regression}
