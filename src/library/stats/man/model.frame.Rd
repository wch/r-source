% File src/library/stats/man/model.frame.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2011 R Core Team
% Distributed under GPL 2 or later

\newcommand{\CRANpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}

\name{model.frame}
\alias{model.frame}
\alias{model.frame.default}
\alias{model.frame.lm}
\alias{model.frame.glm}
\alias{model.frame.aovlist}
\alias{get_all_vars}
\title{Extracting the Model Frame from a Formula or Fit}
\usage{
model.frame(formula, \dots)

\method{model.frame}{default}(formula, data = NULL,
           subset = NULL, na.action = na.fail,
           drop.unused.levels = FALSE, xlev = NULL, \dots)

\method{model.frame}{aovlist}(formula, data = NULL, \dots)

\method{model.frame}{glm}(formula, \dots)

\method{model.frame}{lm}(formula, \dots)

get_all_vars(formula, data, \dots)
}
\arguments{
  \item{formula}{a model \code{\link{formula}} or \code{\link{terms}}
    object or an \R object.}

  \item{data}{a data.frame, list or environment (or object
    coercible by \code{\link{as.data.frame}} to a data.frame),
    containing the variables in \code{formula}.  Neither a matrix nor an
    array will be accepted.}

  \item{subset}{a specification of the rows to be used: defaults to all
    rows. This can be any valid indexing vector (see
    \code{\link{[.data.frame}}) for the rows of \code{data} or if that is not
    supplied, a data frame made up of the variables used in \code{formula}.}

  \item{na.action}{how \code{NA}s are treated.  The default is first,
    any \code{na.action} attribute of \code{data}, second
    a \code{na.action} setting of \code{\link{options}}, and third
    \code{\link{na.fail}} if that is unset.  The \sQuote{factory-fresh}
    default is \code{\link{na.omit}}.  Another possible value is \code{NULL}.}

  \item{drop.unused.levels}{should factors have unused levels dropped?
    Defaults to \code{FALSE}.}

  \item{xlev}{a named list of character vectors giving the full set of levels
    to be assumed for each factor.}

  \item{\dots}{further arguments such as \code{data}, \code{na.action},
    \code{subset}.  Any additional arguments such as \code{offset} and
    \code{weights} which reach the default method are used to create
    further columns in the model frame, with parenthesised names such as
    \code{"(offset)"}.}
}
\description{
  \code{model.frame} (a generic function) and its methods return a
  \code{\link{data.frame}} with the variables needed to use
  \code{formula} and any \code{\dots} arguments.
}
\details{
  Exactly what happens depends on the class and attributes of the object
  \code{formula}.  If this is an object of fitted-model class such as
  \code{"lm"}, the method will either return the saved model frame
  used when fitting the model (if any, often selected by argument
  \code{model = TRUE}) or pass the call used when fitting on to the
  default method.  The default method itself can cope with rather
  standard model objects such as those of class
  \code{"\link[MASS]{lqs}"} from package \CRANpkg{MASS} if no other
  arguments are supplied.

  The rest of this section applies only to the default method.

  If either \code{formula} or \code{data} is already a model frame (a
  data frame with a \code{"terms"} attribute) and the other is missing,
  the model frame is returned.  Unless \code{formula} is a terms object,
  \code{as.formula} and then \code{terms} is called on it.  (If you wish
  to use the \code{keep.order} argument of \code{terms.formula}, pass a
  terms object rather than a formula.)

  Row names for the model frame are taken from the \code{data} argument
  if present, then from the names of the response in the formula (or
  rownames if it is a matrix), if there is one.

  All the variables in \code{formula}, \code{subset} and in \code{\dots}
  are looked for first in \code{data} and then in the environment of
  \code{formula} (see the help for \code{\link{formula}()} for further
  details) and collected into a data frame.  Then the \code{subset}
  expression is evaluated, and it is used as a row index to the data
  frame.  Then the \code{na.action} function is applied to the data frame
  (and may well add attributes).  The levels of any factors in the data
  frame are adjusted according to the \code{drop.unused.levels} and
  \code{xlev} arguments: if \code{xlev} specifies a factor and a
  character variable is found, it is converted to a factor (as from \R
  2.10.0).

  Unless \code{na.action = NULL}, time-series attributes will be removed
  from the variables found (since they will be wrong if \code{NA}s are
  removed).

  Note that \emph{all} the variables in the formula are included in the
  data frame, even those preceded by \code{-}.

  Only variables whose type is raw, logical, integer, real, complex or
  character can be included in a model frame: this includes classed
  variables such as factors (whose underlying type is integer), but
  excludes lists.

  \code{get_all_vars} returns a \code{\link{data.frame}} containing the
  variables used in \code{formula} plus those specified \code{\dots}.
  Unlike \code{model.frame.default}, it returns the input variables and
  not those resulting from function calls in \code{formula}.
}
\value{
  A \code{\link{data.frame}} containing the variables used in
  \code{formula} plus those specified in \code{\dots}.  It will have
  additional attributes, including \code{"terms"} for an object of class
  \code{"\link[=terms.object]{terms}"} derived from \code{formula},
  and possibly \code{"na.action"} giving information on the handling of
  \code{NA}s (which will not be present if no special handling was done,
  e.g. by \code{\link{na.pass}}).
}
\seealso{\code{\link{model.matrix}} for the \sQuote{design matrix},
  \code{\link{formula}} for formulas  and
  \code{\link{expand.model.frame}} for model.frame manipulation.
}
\references{
  Chambers, J. M. (1992)
  \emph{Data for models.}
  Chapter 3 of \emph{Statistical Models in S}
  eds J. M. Chambers and T. J. Hastie, Wadsworth & Brooks/Cole.
}
\examples{
data.class(model.frame(dist ~ speed, data = cars))
}
\keyword{models}
