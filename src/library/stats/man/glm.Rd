\name{glm}
\title{Fitting Generalized Linear Models}
\alias{glm}
\alias{glm.fit}
\alias{weights.glm}
\alias{print.glm}
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
    na.action, start = NULL, etastart, mustart,
    offset, control = glm.control(\dots), model = TRUE,
    method = "glm.fit", x = FALSE, y = TRUE, contrasts = NULL, \dots)

glm.fit(x, y, weights = rep(1, nobs),
        start = NULL, etastart = NULL, mustart = NULL,
        offset = rep(0, nobs), family = gaussian(),
        control = glm.control(), intercept = TRUE)

\method{weights}{glm}(object, type = c("prior", "working"), \dots)
}
\arguments{
  \item{formula}{a symbolic description of the model to be fit.
    The details of model specification are given below.}

  \item{family}{a description of the error distribution and link
    function to be used in the model. This can be a character string
    naming a family function, a family function or the result of a call
    to a family function.  (See \code{\link{family}} for details of
    family functions.)}

  \item{data}{an optional data frame containing the variables
    in the model.  If not found in \code{data}, the variables are taken
    from \code{environment(formula)}, typically the environment from
    which \code{glm} is called.}

  \item{weights}{an optional vector of weights to be used
    in the fitting process.}

  \item{subset}{an optional vector specifying a subset of observations
    to be used in the fitting process.}

  \item{na.action}{a function which indicates what should happen
    when the data contain \code{NA}s.  The default is set by
    the \code{na.action} setting of \code{\link{options}}, and is
    \code{\link{na.fail}} if that is unset.  The \dQuote{factory-fresh}
    default is \code{\link{na.omit}}.}

  \item{start}{starting values for the parameters in the linear predictor.}

  \item{etastart}{starting values for the linear predictor.}

  \item{mustart}{starting values for the vector of means.}

  \item{offset}{this can be used to specify an \emph{a priori}
    known component to be included in the linear predictor
    during fitting.}

  \item{control}{a list of parameters for controlling the fitting
    process.  See the documentation for \code{\link{glm.control}}
    for details.}

  \item{model}{a logical value indicating whether \emph{model frame}
    should be included as a component of the returned value.}

  \item{method}{the method to be used in fitting the model.
    The default method \code{"glm.fit"} uses iteratively reweighted
    least squares (IWLS).  The only current alternative is
    \code{"model.frame"} which returns the model frame and does no fitting.}

  \item{x, y}{For \code{glm}:
    logical values indicating whether the response
    vector and model matrix used in the fitting process
    should be returned as components of the returned value.

    For \code{glm.fit}: \code{x} is a design matrix of dimension \code{n
      * p}, and \code{y} is a vector of observations of length \code{n}.
  }

  \item{contrasts}{an optional list. See the \code{contrasts.arg}
    of \code{model.matrix.default}.}

  \item{object}{an object inheriting from class \code{"glm"}.}
  \item{type}{character, partial matching allowed.  Type of weights to
    extract from the fitted model object.}

  \item{intercept}{logical. Should an intercept be included?}

  \item{\dots}{further arguments passed to or from other methods.}
}
\details{
  A typical predictor has the form \code{response ~ terms} where
  \code{response} is the (numeric) response vector and \code{terms} is a
  series of terms which specifies a linear predictor for \code{response}.
  For \code{binomial} models the response can also be specified as a
  \code{\link{factor}} (when the first level denotes failure and all
  others success) or as a two-column matrix with the columns giving the
  numbers of successes and failures.  A terms specification of the form
  \code{first + second} indicates all the terms in \code{first} together
  with all the terms in \code{second} with duplicates removed.  The terms in
  the formula will be re-ordered so that main effects come first,
  followed by the interactions, all second-order, all third-order and so
  on: to avoid this pass a \code{terms} object as the formula.

  A specification of the form \code{first:second} indicates the
  the set of terms obtained by taking the interactions of
  all terms in \code{first} with all terms in \code{second}.
  The specification \code{first*second} indicates the \emph{cross}
  of \code{first} and \code{second}.
  This is the same as \code{first + second + first:second}.

  \code{glm.fit} and \code{glm.fit.null} are the workhorse functions:
  the former calls the latter for a null model (with no intercept).

  If more than one of \code{etastart}, \code{start} and \code{mustart}
  is specified, the first in the list will be used.

  All of \code{weights}, \code{subset}, \code{offset}, \code{etastart}
  and \code{mustart} are evaluated in the same way as variables in
  \code{formula}, that is first in \code{data} and then in the
  environment of \code{formula}.
}
\value{
  \code{glm} returns an object of class inheriting from \code{"glm"}
  which inherits from the class \code{"lm"}. See later in this section.

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
    in the final iteration of the IWLS fit.}
  \item{fitted.values}{the fitted mean values, obtained by transforming
    the linear predictors by the inverse of the link function.}
  \item{rank}{the numeric rank of the fitted linear model.}
  \item{family}{the \code{\link{family}} object used.}
  \item{linear.predictors}{the linear fit on link scale.}
  \item{deviance}{up to a constant, minus twice the maximized
    log-likelihood.  Where sensible, the constant is chosen so that a
    saturated model has deviance zero.}
  \item{aic}{Akaike's \emph{An Information Criterion}, minus twice the
    maximized log-likelihood plus twice the number of coefficients (so
    assuming that the dispersion is known).}
  \item{null.deviance}{The deviance for the null model, comparable with
    \code{deviance}. The null model will include the offset, and an
    intercept if there is one in the model}
  \item{iter}{the number of iterations of IWLS used.}
  \item{weights}{the \emph{working} weights, that is the weights
    in the final iteration of the IWLS fit.}
  \item{prior.weights}{the case weights initially supplied.}
  \item{df.residual}{the residual degrees of freedom.}
  \item{df.null}{the residual degrees of freedom for the null model.}
  \item{y}{the \code{y} vector used. (It is a vector even for a binomial
    model.)}
  \item{converged}{logical. Was the IWLS algorithm judged to have converged?}
  \item{boundary}{logical. Is the fitted value on the boundary of the
    attainable values?}
  \item{call}{the matched call.}
  \item{formula}{the formula supplied.}
  \item{terms}{the \code{\link{terms}} object used.}
  \item{data}{the \code{data argument}.}
  \item{offset}{the offset vector used.}
  \item{control}{the value of the \code{control} argument used.}
  \item{method}{the name of the fitter function used, in \R always
    \code{"glm.fit"}.}
  \item{contrasts}{(where relevant) the contrasts used.}
  \item{xlevels}{(where relevant) a record of the levels of the factors
    used in fitting.}

  In addition, non-empty fits will have components \code{qr}, \code{R}
  and \code{effects} relating to the final weighted linear fit.

  Objects of class \code{"glm"} are normally of class \code{c("glm",
    "lm")}, that is inherit from class \code{"lm"}, and well-designed
  methods for class \code{"lm"} will be applied to the weighted linear
  model at the final iteration of IWLS.  However, care is needed, as
  extractor functions for class \code{"glm"} such as
  \code{\link{residuals}} and \code{weights} do \bold{not} just pick out
  the component of the fit with the same name.

  If a \code{\link{binomial}} \code{glm} model is specified by giving a
  two-column response, the weights returned by \code{prior.weights} are
  the total numbers of cases (factored by the supplied case weights) and
  the component \code{y} of the result is the proportion of successes.
}
\seealso{
  \code{\link{anova.glm}}, \code{\link{summary.glm}}, etc. for
  \code{glm} methods,
  and the generic functions \code{\link{anova}}, \code{\link{summary}},
  \code{\link{effects}}, \code{\link{fitted.values}},
  and \code{\link{residuals}}. Further, \code{\link{lm}} for
  non-generalized \emph{linear} models.

  \code{\link[datasets]{esoph}}, \code{\link[datasets]{infert}} and
  \code{\link{predict.glm}} have examples of fitting binomial glms.
}
\author{
  The original \R implementation of \code{glm} was written by Simon
  Davies working for Ross Ihaka at the University of Auckland, but has
  since been extensively re-written by members of the R Core team.

  The design was inspired by the S function of the same name described
  in Hastie \& Pregibon (1992).
}
\references{
  Dobson, A. J. (1990)
  \emph{An Introduction to Generalized Linear Models.}
  London: Chapman and Hall.

  Hastie, T. J. and Pregibon, D. (1992)
  \emph{Generalized linear models.}
  Chapter 6 of \emph{Statistical Models in S}
  eds J. M. Chambers and T. J. Hastie, Wadsworth \& Brooks/Cole.

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
glm.D93 <- glm(counts ~ outcome + treatment, family=poisson())
anova(glm.D93)
summary(glm.D93)

## an example with offsets from Venables & Ripley (2002, p.189)
\dontshow{"anorexia" <-
structure(list(Treat = structure(c(2, 2, 2, 2, 2, 2, 2, 2, 2,
2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1,
1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
1, 1, 1, 1, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3
), .Label = c("CBT", "Cont", "FT"), class = "factor"), Prewt = c(80.7,
89.4, 91.8, 74, 78.1, 88.3, 87.3, 75.1, 80.6, 78.4, 77.6, 88.7,
81.3, 78.1, 70.5, 77.3, 85.2, 86, 84.1, 79.7, 85.5, 84.4, 79.6,
77.5, 72.3, 89, 80.5, 84.9, 81.5, 82.6, 79.9, 88.7, 94.9, 76.3,
81, 80.5, 85, 89.2, 81.3, 76.5, 70, 80.4, 83.3, 83, 87.7, 84.2,
86.4, 76.5, 80.2, 87.8, 83.3, 79.7, 84.5, 80.8, 87.4, 83.8, 83.3,
86, 82.5, 86.7, 79.6, 76.9, 94.2, 73.4, 80.5, 81.6, 82.1, 77.6,
83.5, 89.9, 86, 87.3), Postwt = c(80.2, 80.1, 86.4, 86.3, 76.1,
78.1, 75.1, 86.7, 73.5, 84.6, 77.4, 79.5, 89.6, 81.4, 81.8, 77.3,
84.2, 75.4, 79.5, 73, 88.3, 84.7, 81.4, 81.2, 88.2, 78.8, 82.2,
85.6, 81.4, 81.9, 76.4, 103.6, 98.4, 93.4, 73.4, 82.1, 96.7,
95.3, 82.4, 72.5, 90.9, 71.3, 85.4, 81.6, 89.1, 83.9, 82.7, 75.7,
82.6, 100.4, 85.2, 83.6, 84.6, 96.2, 86.7, 95.2, 94.3, 91.5,
91.9, 100.3, 76.7, 76.8, 101.6, 94.9, 75.2, 77.8, 95.5, 90.7,
92.5, 93.8, 91.7, 98)), .Names = c("Treat", "Prewt", "Postwt"
), class = "data.frame", row.names = c("1", "2", "3", "4", "5",
"6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16",
"17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27",
"28", "29", "30", "31", "32", "33", "34", "35", "36", "37", "38",
"39", "40", "41", "42", "43", "44", "45", "46", "47", "48", "49",
"50", "51", "52", "53", "54", "55", "56", "57", "58", "59", "60",
"61", "62", "63", "64", "65", "66", "67", "68", "69", "70", "71",
"72"))}
%% "else" (not dontshow)
\dontrun{
## Need the anorexia data from a recent version of the package 'MASS':
library(MASS)
}
anorex.1 <- glm(Postwt ~ Prewt + Treat + offset(Prewt),
            family = gaussian, data = anorexia)
summary(anorex.1)

# A Gamma example, from McCullagh & Nelder (1989, pp. 300-2)
clotting <- data.frame(
    u = c(5,10,15,20,30,40,60,80,100),
    lot1 = c(118,58,42,35,27,25,21,19,18),
    lot2 = c(69,35,26,21,18,16,13,12,12))
summary(glm(lot1 ~ log(u), data=clotting, family=Gamma))
summary(glm(lot2 ~ log(u), data=clotting, family=Gamma))

\dontrun{
## for an example of the use of a terms object as a formula
demo(glm.vr)
}}
\keyword{models}
\keyword{regression}
