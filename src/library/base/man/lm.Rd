\name{lm}
\title{Fitting Linear Models}
\usage{
lm(formula, data, subset, weights, na.action=na.omit,
   method="qr", model=TRUE, singular.ok = TRUE)

lm.fit (x, y,    method = "qr", tol = 1e-7, \dots)
lm.wfit(x, y, w, method = "qr", tol = 1e-7, \dots)
lm.fit.null (x, y,    method = "qr", tol = 1e-7, \dots)
lm.wfit.null(x, y, w, method = "qr", tol = 1e-7, \dots)
}
\alias{lm}
\alias{lm.fit}
\alias{lm.wfit}
\alias{lm.fit.null}
\alias{lm.wfit.null}
\arguments{
\item{formula}{a symbolic description of the model to be fit.
The details of model specification are given below.}
\item{data}{an optional data frame containing the variables
in the model.  By default the variables are taken from
the environment which \code{lm} is called from.}
\item{subset}{an optional vector specifying a subset of observations
to be used in the fitting process.}
\item{weights}{an optional vector of weights to be used
in the fitting process. If specified, weighted least squares is used
with weights \code{weights} (that is, minimizing \code{sum(w*e^2)});
otherwise ordinary least squares is used.}
\item{na.action}{a function which indicates what should happen
when the data contain \code{NA}s.  The default action (\code{na.omit})
is to omit any incomplete observations.
The alternative action \code{na.fail} causes \code{lm} to
print an error message and terminate if there are any incomplete
observations.}
\item{model}{logical.  If \code{TRUE} (default), the model.frame is also
  returned.}
\item{singular.ok}{logical, defaulting to
  \code{TRUE}. \emph{\code{FALSE} is not yet implemented}.}
\item{method}{currently, only \code{method="qr"} is supported.}
\item{tol}{tolerance for the \code{\link{qr}} decomposition.  Default is 1e-7.}
\item{\dots}{currently disregarded.}
}
\description{
    \code{lm} is used to fit linear models.
    It can be used to carry out regression,
    single stratum analysis of variance and
    analysis of covariance.
}
\details{
Models for \code{lm} are specified symbolically.
A typical model has the form
\code{response ~ terms} where \code{response} is the (numeric)
response vector and \code{terms} is a series of terms which
specifies a linear predictor for \code{response}.
A terms specification of the form \code{first+second}
indicates all the terms in \code{first} together
with all the terms in \code{second} with duplicates
removed.
A specification of the form \code{first:second} indicates the
the set of terms obtained by taking the interactions of
all terms in \code{first} with all terms in \code{second}.
The specification \code{first*second} indicates the \emph{cross}
of \code{first} and \code{second}.
This is the same as \code{first+second+first:second}.
}
\value{
  \code{lm} returns an object of \code{\link{class}} \code{"lm"}.

The functions \code{summary} and \code{\link{anova}} are used to
obtain and print a summary and analysis of variance table of the results.
The generic accessor functions \code{coefficients},
\code{effects}, \code{fitted.values} and \code{residuals}
extract various useful features of the value returned by \code{lm}.
}
\seealso{
  \code{\link{summary.lm}} for summaries and \code{\link{anova.lm}} for
  the ANOVA table. \code{\link{aov}} for a difference interface.

  The generic functions \code{\link{coefficients}}, \code{\link{effects}},
  \code{\link{residuals}}, \code{\link{fitted.values}};
  \code{\link{lm.influence}} for regression diagnostics, and
  \code{\link{glm}} for \bold{generalized} linear models.
}
\examples{
## Annette Dobson (1990) "An Introduction to Statistical Modelling".
## Page 9: Plant Weight Data.
ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
group <- gl(2,10,20,labels=c("Ctl","Trt"))
weight <- c(ctl,trt)
anova(lm.D9 <- lm(weight~group))
summary(lm.D90 <- lm(weight ~ group -1))# omitting intercept
summary(resid(lm.D9) - resid(lm.D90)) #- residuals almost identical

plot(lm.D9)# Residuals, Fitted,..
}
\keyword{regression}
