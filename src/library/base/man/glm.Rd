\name{glm}
\title{Fitting Generalized Linear Models}
\usage{
glm(formula, family = gaussian, data, weights = NULL, subset = NULL,
    na.action, start = NULL, offset = NULL,
    control = glm.control(epsilon=0.0001, maxit=10, trace=FALSE),
    model = TRUE, method = "glm.fit", x = FALSE, y = TRUE,
    contrasts = NULL, ...)
glm.control(epsilon = 0.0001, maxit = 10, trace = FALSE)
glm.fit(x, y, weights = rep(1, nrow(x)),
        start = NULL, etastart  =  NULL, mustart = NULL,
        offset = rep(0, nrow(x)),
        family = gaussian(), control = glm.control(),
        intercept = TRUE)
}
\alias{glm}
\alias{glm.control}
\alias{glm.fit}
\alias{glm.fit.null}
\arguments{
  \item{formula}{a symbolic description of the model to be fit.
    The details of model specification are given below.}

  \item{family}{a description of the error distribution and link
    function to be used in the model.
    See \code{\link{family}} for details.}

  \item{data}{an optional data frame containing the variables
    in the model.  By default the variables are taken from
    the environment from which \code{glm} is called.}

  \item{weights}{an optional vector of weights to be used
    in the fitting process.}

  \item{subset}{an optional vector specifying a subset of observations
    to be used in the fitting process.}

  \item{na.action}{a function which indicates what should happen
    when the data contain \code{NA}s.  The default is set by
    the \code{na.action} setting of \code{\link{options}}, and is
    \code{\link{na.fail}} if that is unset.  The ``factory-fresh''
    default is \code{\link{na.omit}}.}

  \item{start}{starting values for the parameters in the linear predictor.}

  \item{etastart}{starting values for the linear predictor.}

  \item{mustart}{starting values for the vector of means.}

  \item{offset}{this can be used to specify an \emph{a priori}
    known component to be included in the linear predictor
    during fitting.}

  \item{control}{a list of parameters for controlling the fitting
    process.  See the documentation for \code{\link{glm.control}} for details.}

  \item{model}{a logical value indicating whether \emph{model frame}
    should be included as a component of the returned value.}

  \item{method}{the method to be used in fitting the model.
    The default (and presently only) method \code{glm.fit}
    uses iteratively reweighted least squares.}

  \item{x, y}{logical values indicating whether the response
    vector and model matrix used in the fitting process
    should be returned as components of the returned value.}

  \item{contrasts}{an optional list. See the \code{contrasts.arg}
    of \code{model.matrix.default}.}
}
\description{
  \code{glm} is used to fit generalized linear models.

  Models for \code{glm} are specified by giving
  a symbolic description of the linear predictor and
  a description of the error distribution.
}
\details{
  A typical predictor has the form \code{response ~ terms} where
  \code{response} is the (numeric) response vector and \code{terms} is a
  series of terms which specifies a linear predictor for \code{response}.
  For \code{binomial} models the response can also be specified as a
  \code{\link{factor}} (when the first level denotes failure and all
  others success) or as a two-column matrix with the columns giving the
  numbers of successes and failures.  A terms specification of the form
  \code{first+second} indicates all the terms in \code{first} together
  with all the terms in \code{second} with duplicates removed.

  A specification of the form \code{first:second} indicates the
  the set of terms obtained by taking the interactions of
  all terms in \code{first} with all terms in \code{second}.
  The specification \code{first*second} indicates the \emph{cross}
  of \code{first} and \code{second}.
  This is the same as \code{first+second+first:second}.
}
\value{
  \code{glm} returns an object of class \code{glm}
  which inherits from the class \code{lm}.
  The function \code{\link{summary}} (i.e., \code{\link{summary.glm}}) can
  be used to obtain or print a summary of the results and the function
  \code{\link{anova}} (i.e., \code{\link{anova.glm}})
  to produce an analysis of variance table.

  The generic accessor functions \code{\link{coefficients}},
  \code{effects}, \code{fitted.values} and \code{residuals} can be used to
  extract various useful features of the value returned by \code{glm}.
}
\seealso{
  \code{\link{anova.glm}}, \code{\link{summary.glm}}, etc. for
  \code{glm} methods,
  and the generic functions \code{\link{anova}}, \code{\link{summary}},
  \code{\link{effects}}, \code{\link{fitted.values}},
  and \code{\link{residuals}}. Further, \code{\link{lm}} for
  non-generalized \emph{linear} models.
}
\note{
  Offsets specified by \code{offset} will not be included in predictions
  by \code{\link{predict.glm}}, whereas those specified by an offset term
  in the formula will be.
}
\examples{
## Annette Dobson (1990) "An Introduction to Generalized Linear Models".
## Page 93: Randomized Controlled Trial :
counts <- c(18,17,15,20,10,20,25,13,12)
outcome <- gl(3,1,9)
treatment <- gl(3,3)
print(d.AD <- data.frame(treatment, outcome, counts))
glm.D93 <- glm(counts ~ outcome + treatment, family=poisson())
anova(glm.D93)
summary(glm.D93)

## an example with offsets from Venables & Ripley (1999, pp.217-8)
\testonly{"anorexia" <-
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
%% "else" (not testonly)
\dontrun{
 ## Need the anorexia data from a 1999 version of the package MASS:
 library(MASS)
 data(anorexia)
}
anorex.1 <- glm(Postwt ~ Prewt + Treat + offset(Prewt),
            family = gaussian, data = anorexia)
summary(anorex.1)
\testonly{
## these are the same -- example from Jim Lindsey
y <- rnorm(20)
y1 <- y[-1]; y2 <- y[-20]
summary(glm(y1 - y2 ~ 1))
summary(glm(y1 ~ offset(y2)))
}
}
\keyword{models}
\keyword{regression}
