% file step.Rd
% copyright (C) 1998 B. D. Ripley
%
\name{step}
\title{
Choose a model by AIC in a Stepwise Algorithm
}
\usage{
step(object, scope, scale=0, direction=c("both", "backward", "forward"), 
	trace=1, keep=NULL, steps=1000, k=2, \dots)
}
\alias{step}
\arguments{
\item{object}{
  an object representing a model of an appropriate class.
  This is used as the initial model in the stepwise search.}
\item{scope}{
  defines the range of models examined in the stepwise search.}
\item{scale}{
  used in the definition of the AIC statistic for selecting the models,
  currently only for \code{\link{lm}}, \code{\link{aov}} and
  \code{\link{glm}} models.}
\item{direction}{
  the mode of stepwise search, can be one of \code{"both"}, \code{"backward"}, 
  or \code{"forward"}, with a default of \code{"both"}. 
  If the \code{scope} argument is missing, 
  the default for \code{direction} is \code{"backward"}.}
\item{trace}{
  if positive, information is printed during the running of \code{step}.}
\item{keep}{
  a filter function whose input is a fitted model object and the 
  associated \code{AIC} statistic, and whose output is arbitrary. 
  Typically \code{keep} will select a subset of the components of 
  the object and return them. The default is not to keep anything.}
\item{steps}{
  the maximum number of steps to be considered.  The default is 1000
  (essentially as many as required).  It is typically used to stop the
  process early.}
\item{k}{
  the multiple of the number of degrees of freedom used for the penalty.
  Only \code{k=2} gives the genuine AIC: \code{k = log(n)} is sometimes
  referred to as BIC or SBC.}
\item{\dots}{any additional arguments to \code{\link{extractAIC}}.}
}
\value{
  the stepwise-selected model is returned, with up to two additional
  components.  There is an \code{"anova"} component corresponding to the
  steps taken in the search, as well as a \code{"keep"} component if the
  \code{keep=} argument was supplied in the call. The
  \code{"Resid. Dev"} column of the analysis of deviance table refers
  to a constant minus twice the maximized log likelihood: it will be a
  deviance only in cases where a saturated model is well-defined
  (thus excluding \code{lm}, \code{aov} and \code{survreg} fits, for example).
}
\description{
  \code{step} uses \code{\link{add1}} and \code{\link{drop1}}
  repeatedly; it will work for any method for which they work, and that
  is determined by having a valid method for \code{\link{extractAIC}}.
  When the additive constant can be chosen so that AIC is equal to
  Mallows' Cp, this is done and the tables are labelled appropriately.

  There is a potential problem in using \code{\link{glm}} fits with a variable
  \code{scale}, as in that case the deviance is not simply related to the
  maximized log-likelihood. The function \code{\link{extractAIC.glm}} makes the
  appropriate adjustment for a \code{gaussian} family, but may need to be
  amended for other cases. (The \code{binomial} and \code{poisson}
  families have fixed \code{scale} by default and do not correspond
  to a particular maximum-likelihood problem for variable \code{scale}.)
}
\note{This function differs considerably from the function in S, which uses a
  number of approximations and does not compute the correct AIC.}
\seealso{
\code{\link{add1}}, \code{\link{drop1}}
}
\author{B.D. Ripley}
\examples{
example(lm)
step(lm.D9)  

data(swiss)
summary(lm1 <- lm(Fertility ~ ., data = swiss))
slm1 <- step(lm1)
summary(slm1)
slm1 $ anova
}
\keyword{models}
