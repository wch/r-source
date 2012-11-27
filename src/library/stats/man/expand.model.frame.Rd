% File src/library/stats/man/expand.model.frame.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{expand.model.frame}
\alias{expand.model.frame}
\title{Add new variables to a model frame}
\description{
  Evaluates new variables as if they had been part of the formula of the
  specified model.  This ensures that the same \code{na.action} and
  \code{subset} arguments are applied and allows, for example, \code{x}
  to be recovered for a model using \code{sin(x)} as a predictor.
}
\usage{
expand.model.frame(model, extras,
                   envir = environment(formula(model)),
                   na.expand = FALSE)
}
\arguments{
  \item{model}{a fitted model}
  \item{extras}{one-sided formula or vector of character strings
    describing new variables to be added}
  \item{envir}{an environment to evaluate things in}
  \item{na.expand}{logical; see below}
}
\details{
  If \code{na.expand = FALSE} then \code{NA} values in the extra variables
  will be passed to the \code{na.action} function used in
  \code{model}.  This may result in a shorter data frame (with
  \code{\link{na.omit}}) or an error (with \code{\link{na.fail}}).  If
  \code{na.expand = TRUE} the returned data frame will have precisely the
  same rows as \code{model.frame(model)}, but the columns corresponding to
  the extra variables may contain \code{NA}.
}
\value{
  A data frame.
}
\seealso{\code{\link{model.frame}}, \code{\link{predict}}}

\examples{
model <- lm(log(Volume) ~ log(Girth) + log(Height), data = trees)
expand.model.frame(model, ~ Girth) # prints data.frame like

dd <- data.frame(x = 1:5, y = rnorm(5), z = c(1,2,NA,4,5))
model <- glm(y ~ x, data = dd, subset = 1:4, na.action = na.omit)
expand.model.frame(model, "z", na.expand = FALSE) # = default
expand.model.frame(model, "z", na.expand = TRUE)
}
\keyword{manip}
\keyword{regression}
