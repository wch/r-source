% File src/library/stats/man/formula.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{formula}
\alias{formula}
\alias{formula.default}
\alias{formula.formula}
\alias{formula.terms}
\alias{formula.data.frame}
\alias{as.formula}
\alias{print.formula}
\alias{[.formula}

\title{Model Formulae}
\usage{
formula(x, \dots)
as.formula(object, env = parent.frame())

\method{print}{formula}(x, showEnv = !identical(e, .GlobalEnv), \dots)
}
\description{
  The generic function \code{formula} and its specific methods provide a
  way of extracting formulae which have been included in other objects.

  \code{as.formula} is almost identical, additionally preserving
  attributes when \code{object} already inherits from
  \code{"formula"}.
}
\arguments{
  \item{x, object}{\R object.}
  \item{\dots}{further arguments passed to or from other methods.}
  \item{env}{the environment to associate with the result, if not
    already a formula.}
  \item{showEnv}{logical indicating if the environment should be printed
    as well.}
}
\details{
  The models fit by, e.g., the \code{\link{lm}} and \code{\link{glm}} functions
  are specified in a compact symbolic form.
  The \code{~} operator is basic in the formation of such models.
  An expression of the form \code{y ~ model} is interpreted
  as a specification that the response \code{y} is modelled
  by a linear predictor specified symbolically by \code{model}.
  Such a model consists of a series of terms separated
  by \code{+} operators.
  The terms themselves consist of variable and factor
  names separated by \code{:} operators.
  Such a term is interpreted as the interaction of
  all the variables and factors appearing in the term.

  In addition to \code{+} and \code{:}, a number of other operators are
  useful in model formulae.  The \code{*} operator denotes factor
  crossing: \code{a*b} interpreted as \code{a+b+a:b}.  The \code{^}
  operator indicates crossing to the specified degree.  For example
  \code{(a+b+c)^2} is identical to \code{(a+b+c)*(a+b+c)} which in turn
  expands to a formula containing the main effects for \code{a},
  \code{b} and \code{c} together with their second-order interactions.
  The \code{\%in\%} operator indicates that the terms on its left are
  nested within those on the right.  For example \code{a + b \%in\% a}
  expands to the formula \code{a + a:b}.  The \code{-} operator removes
  the specified terms, so that \code{(a+b+c)^2 - a:b} is identical to
  \code{a + b + c + b:c + a:c}.  It can also used to remove the
  intercept term: when fitting a linear model \code{y ~ x - 1} specifies
  a line through the origin.  A model with no intercept can be also
  specified as \code{y ~ x + 0} or \code{y ~ 0 + x}.

  While formulae usually involve just variable and factor
  names, they can also involve arithmetic expressions.
  The formula \code{log(y) ~ a + log(x)} is quite legal.
  When such arithmetic expressions involve
  operators which are also used symbolically
  in model formulae, there can be confusion between
  arithmetic and symbolic operator use.

  To avoid this confusion, the function \code{\link{I}()}
  can be used to bracket those portions of a model
  formula where the operators are used in their
  arithmetic sense.  For example, in the formula
  \code{y ~ a + I(b+c)}, the term \code{b+c} is to be
  interpreted as the sum of \code{b} and \code{c}.

  Variable names can be quoted by backticks \code{`like this`} in
  formulae, although there is no guarantee that all code using formulae
  will accept such non-syntactic names.

  Most model-fitting functions accept formulae with right-hand-side
  including the function \code{\link{offset}} to indicate terms with a
  fixed coefficient of one.  Some functions accept other
  \sQuote{specials} such as \code{strata} or \code{cluster} (see the
  \code{specials} argument of \code{\link{terms.formula})}.

  There are two special interpretations of \code{.} in a formula.  The
  usual one is in the context of a \code{data} argument of model
  fitting functions and means \sQuote{all columns not otherwise in the
  formula}: see \code{\link{terms.formula}}.  In the context of
  \code{\link{update.formula}}, \bold{only}, it means \sQuote{what was
    previously in this part of the formula}.

  When \code{formula} is called on a fitted model object, either a
  specific method is used (such as that for class \code{"nls"}) or the
  default method.  The default first looks for a \code{"formula"}
  component of the object (and evaluates it), then a \code{"terms"}
  component, then a \code{formula} parameter of the call (and evaluates
  its value) and finally a \code{"formula"} attribute.

  There is a \code{formula} method for data frames.  If there is only
  one column this forms the RHS with an empty LHS.  For more columns,
  the first column is the LHS of the formula and the remaining columns
  separated by \code{+} form the RHS.
}
\section{Environments}{
  A formula object has an associated environment, and
  this environment (rather than the parent
  environment) is used by \code{\link{model.frame}} to evaluate variables
  that are not found in the supplied \code{data} argument.

  Formulas created with the \code{~} operator use the
  environment in which they were created.  Formulas created with
  \code{as.formula} will use the \code{env} argument for their
  environment.
}
\value{
  All the functions above produce an object of class \code{"formula"}
  which contains a symbolic model formula.
}
\references{
  Chambers, J. M. and Hastie, T. J. (1992)
  \emph{Statistical models.}
  Chapter 2 of \emph{Statistical Models in S}
  eds J. M. Chambers and T. J. Hastie, Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{I}}, \code{\link{offset}}.

  For formula manipulation: \code{\link{terms}}, and \code{\link{all.vars}};
  for typical use: \code{\link{lm}}, \code{\link{glm}}, and
  \code{\link{coplot}}.
}
\examples{
class(fo <- y ~ x1*x2) # "formula"
fo
typeof(fo)  # R internal : "language"
terms(fo)

environment(fo)
environment(as.formula("y ~ x"))
environment(as.formula("y ~ x", env = new.env()))


## Create a formula for a model with a large number of variables:
xnam <- paste0("x", 1:25)
(fmla <- as.formula(paste("y ~ ", paste(xnam, collapse= "+"))))
}
\keyword{models}
