\name{glm}
\title{Fitting Generalized Linear Models}
\usage{
glm(formula, family=gaussian, data, weights, subset,
        na.action=na.fail, start=NULL, offset=NULL,
        control=glm.control(epsilon=0.0001, maxit=10,
                trace=F),
        model=T, method=glm.fit, x=F, y=T)

summary(glm.obj, dispersion=NULL, correlation=FALSE,
        na.action=na.omit)
anova(glm.obj, \dots)

coefficients(glm.obj)
deviance(glm.obj)
df.residual(glm.obj)
effects(glm.obj)
family(glm.obj)
fitted.values(glm.obj)
residuals(glm.obj, type="deviance")

glm.control(epsilon=0.0001, maxit=10, trace=FALSE)
glm.fit(x, y, weights=rep(1, nrow(x)),
        start=NULL, offset=rep(0, nrow(x)),
        family=gaussian(), control=glm.control(),
        intercept=TRUE)
}
\alias{glm}
\alias{glm.control}
\alias{glm.fit}
\alias{summary.glm}
\alias{anova.glm}
\alias{coefficients.glm}
\alias{deviance.glm}
\alias{df.residual.glm}
\alias{effects.glm}
\alias{family.glm}
\alias{fitted.values.glm}
\alias{residuals.glm}
\alias{print.glm}
\alias{print.anova.glm}
\alias{print.summary.glm}
\arguments{
\item{formula}{a symbolic description of the model to be fit.
The details of model specification are given below.}
\item{family}{a description of the error distribution and link
function to be used in the model.
See \code{\link{family}} for details.}
\item{data}{an optional data frame containing the variables
in the model.  By default the variables are taken from
the environment which \code{lm} is called from.}
\item{weights}{an optional vector of weights to be used
in the fitting process.}
\item{subset}{an optional vector specifying a subset of observations
to be used in the fitting process.}
\item{na.action}{a function which indicates what should happen
when the data contain \code{NA}s.  The default action (\code{na.omit})
is to omit any incomplete observations.
The alternative action \code{na.fail} causes \code{lm} to
print an error message and terminate if there are any incomplete
observations.}
\item{start}{starting values for the parameters in the
linear predictor.}
\item{offset}{this can be used to specify an a-priori
known component to be included in the linear predictor
during fitting.}
\item{control}{a list of parameters for controlling the fitting
process.  See the documentation for \code{\link{glm.control}} for details.}
\item{model}{a logical value indicating whether \emph{model frame}
should be included as a component of the returned value.}
\item{method}{the method to be used in fitting the model.
The default (and presently only) method \code{glm.fit}
uses iteratively reweighted least squares.}
\item{x,y}{logical values indicating whether the response
vector and design matrix used in the fitting process
should be returned as components of the returned value.}
\item{glm.obj}{an object of class \code{glm}.}
\item{dispersion}{the dispersion parameter for the fitting family.
By default the dispersion parameter is obtained from
\code{glm.obj}.}
\item{correlation}{should the correlation matrix of the estimated parameters
be printed.}
\item{type}{the type of residuals which should be returned.  The alternatives are:
\code{"deviance"}, \code{"pearson"}, \code{"working"}, \code{"response"}.}
}
\description{
\code{glm} is used to fit generalized linear models.

Models for \code{glm} are specified by giving
a symbolic description of the linear predictor and
a description of the error distribution.
A typical predictor has the form
\code{reponse ~ terms} where \code{response} is the (numeric)
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
\code{glm} returns an object of class \code{glm}
which inherits from the class \code{lm}.
The function \code{summary} can be used to obtain or print
a summary of the results and the function \code{anova}
and be used to produce and analysis of variance table.
The generic accessor functions \code{coefficients},
\code{effects}, \code{fitted.values} and \code{residuals}
can be used to extract various useful features of the
value returned by \code{glm}.
}
\seealso{
\code{\link{anova}}, \code{\link{coefficients}}, \code{\link{effects}},
\code{\link{fitted.values}},
\code{\link{lm}},
\code{\link{residuals}}, \code{\link{summary}}.
}
\examples{
## Annette Dobson (1990) "An Introduction to Statistical Modelling".
## Page 93: Randomized Controlled Trial :
counts <- c(18,17,15,20,10,20,25,13,12)
outcome <- gl(3,1,9)
treatment <- gl(3,3,9)
print(d.AD <- data.frame(treatment, outcome, counts))
glm.D93 <- glm(counts ~ outcome + treatment, family=poisson())
anova(glm.D93)
summary(glm.D93)
}
\keyword{models}
\keyword{regression}

