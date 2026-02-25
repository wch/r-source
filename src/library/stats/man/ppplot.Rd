% File src/library/stats/man/ppplot.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2026 R Core Team
% Distributed under GPL 2 or later

\name{ppplot}
\alias{ppplot}
\title{
Probability-probability Plots
}
\description{
\code{ppplot} produces a probability-probability (P-P) plot of two numerical
variables. If \code{conf.level} is given, an estimate and corresponding 
confidence band for the P-P curve under a 
distribution-free semiparametric model is plotted.
}
\usage{
ppplot(x, y, plot.it = TRUE, 
       xlab = paste("Cumulative probabilities for", deparse1(substitute(x))),
       ylab = paste("Cumulative probabilities for", deparse1(substitute(y))), 
       main = "P-P plot", ..., conf.level = NULL, 
       conf.args = list(link = "logit", type = "Wald", col = NA, border = NULL))
}
\arguments{
  \item{x}{the first sample for \code{ppplot}, a numerical variable.
}
  \item{y}{the second sample for \code{ppplot}, a numerical variable.
}
  \item{plot.it}{logical. Should the result be plotted?
}
  \item{xlab, ylab}{the \code{xlab} and \code{ylab} refer to the y
          and x axes respectively.
}
  \item{main}{a main title for the plot.}
  \item{\dots}{graphical parameters.
}
  \item{conf.level}{confidence level of the band. The default, \code{NULL}, does not
          lead to the computation of a confidence band.
}
  \item{conf.args}{list of arguments defining confidence band computation and
         visualisation: \code{link} defines the link function of a
         distribution-free semiparametric model, \code{type} specifies the
         statistical concept the confidence band is derived from; see
         \code{\link[stats]{free1way}} for other options. The remaining
         elements govern how the band is plotted.
}
}
\details{

For independent two samples, denoted \eqn{x} and \eqn{y}, 
the function produces a probability-probability plot \bibcitep{R:Wilk+Gnanadesikan:1968} of pairs
\eqn{(\hat{F}_{x}(z), \hat{F}_{y}(z))} for observed data \eqn{z = (x, y)}.

If the data generating process follows a model where the two distribution
functions, after appropriate transformation, are horizontally shifted
versions of each other, the probability-probability curve is a simple function of this shift and
confidence bands can be obtained from a confidence interval for this shift
parameter, see \code{\link[stats]{free1way}} for the model and
\bibcitet{R:Sewak+Hothorn:2023} for the connection to ROC curves.

Substantial deviations of the empirical (step function) from the theoretical
(smooth) curve indicates lack of fit of the semiparametric model.

}
\value{
An object of class \code{stepfun}.
}
\references{
  \bibshow{*}
}
\examples{

## make example reproducible
if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) 
    runif(1)
R.seed <- get(".Random.seed", envir = .GlobalEnv)
set.seed(29)

## well-fitting logistic model
nd <- data.frame(groups = gl(2, 50, labels = paste0("G", 1:2)))
nd$y <- rlogis(nrow(nd), location = c(0, 2)[nd$groups])
with(with(nd, split(y, groups)),
     ppplot(G1, G2, conf.level = .95,
            conf.args = list(link = "logit", type = "Wald", col = 2)))
# with appropriate Wilcoxon test and log-odds ratio
coef(ft <- free1way(y ~ groups, data = nd))
# the model-based probability-probability curve
prb <- 1:99 / 100
points(prb,  plogis(qlogis(prb) - coef(ft)), pch = 3)

## the corresponding model-based receiver operating characteristic (ROC)
## curve, see Sewak and Hothorn (2023)
plot(prb,  plogis(qlogis(1 - prb) - coef(ft), lower.tail = FALSE),
     xlab = "1 - Specificity", ylab = "Sensitivity", type = "l", 
     main = "ROC Curve")
abline(a = 0, b = 1, col = "lightgrey")
# with confidence band
lines(prb, plogis(qlogis(1 - prb) - confint(ft, test = "Rao")[1], 
      lower.tail = FALSE), lty = 3)
lines(prb, plogis(qlogis(1 - prb) - confint(ft, test = "Rao")[2], 
      lower.tail = FALSE), lty = 3)
# and corresponding area under the ROC curve (AUC)
# with score confidence interval
coef(ft, what = "AUC")
confint(ft, test = "Rao", what = "AUC")

## ill-fitting normal model
nd$y <- rnorm(nrow(nd), mean = c(0, .5)[nd$groups], sd = c(1, 1.5)[nd$groups])
with(with(nd, split(y, groups)),
     ppplot(G1, G2, conf.level = .95,
            conf.args = list(link = "probit", type = "Wald", col = 2)))
# inappropriate probit model
coef(free1way(y ~ groups, data = nd, link = "probit"))

assign(".Random.seed", R.seed, envir = .GlobalEnv)
}
\keyword{hplot}
\keyword{distribution}
