% File src/library/stats/man/update.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2011 R Core Team
% Distributed under GPL 2 or later

\name{update}
\title{Update and Re-fit a Model Call}
\alias{update}
\alias{update.default}
\alias{getCall}
\alias{getCall.default}
\usage{
update(object, \dots)
\method{update}{default}(object, formula., \dots, evaluate = TRUE)

getCall(x, \dots)
}
\arguments{
 \item{object, x}{An existing fit from a model function such as \code{lm},
   \code{glm} and many others.}
 \item{formula.}{Changes to the formula -- see \code{update.formula} for
   details.}
 \item{\dots}{Additional arguments to the call, or arguments with
   changed values. Use \code{name = NULL} to remove the argument \code{name}.}
 \item{evaluate}{If true evaluate the new call else return the call.}
}
\description{
  \code{update} will update and (by default) re-fit a model.  It does this
  by extracting the call stored in the object, updating the call and (by
  default) evaluating that call.  Sometimes it is useful to call
  \code{update} with only one argument, for example if the data frame has
  been corrected.

  \dQuote{Extracting the call} in \code{update()} and similar functions
  uses \code{getCall()} which itself is a (S3) generic function with a
  default method that simply gets \code{x$call}.

  Because of this, \code{update()} will often work (via its default
  method) on new model classes, either automatically, or by providing a
  simple \code{getCall()} method for that class.
}
\value{
  If \code{evaluate = TRUE} the fitted object, otherwise the updated call.
}
\seealso{
  \code{\link{update.formula}}
}
\references{
  Chambers, J. M. (1992)
  \emph{Linear models.}
  Chapter 4 of \emph{Statistical Models in S}
  eds J. M. Chambers and T. J. Hastie, Wadsworth & Brooks/Cole.
}
\examples{
oldcon <- options(contrasts = c("contr.treatment", "contr.poly"))
## Annette Dobson (1990) "An Introduction to Generalized Linear Models".
## Page 9: Plant Weight Data.
ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
group <- gl(2, 10, 20, labels = c("Ctl", "Trt"))
weight <- c(ctl, trt)
lm.D9 <- lm(weight ~ group)
lm.D9
summary(lm.D90 <- update(lm.D9, . ~ . - 1))
options(contrasts = c("contr.helmert", "contr.poly"))
update(lm.D9)
getCall(lm.D90)  # "through the origin"

options(oldcon)
}
\keyword{models}
