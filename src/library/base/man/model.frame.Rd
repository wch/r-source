\name{model.frame}
\alias{model.frame}
\alias{model.frame.default}
\alias{model.frame.lm}
\alias{model.frame.glm}
\alias{model.frame.aovlist}
\title{Extracting the ``Environment'' of a Model Formula}
\usage{
model.frame(formula, \dots)

\method{model.frame}{default}(formula, data = NULL,
           subset = NULL, na.action = na.fail,
           drop.unused.levels = FALSE, xlev = NULL, \dots)

\method{model.frame}{aovlist}(formula, data = NULL, \dots)

\method{model.frame}{glm}(formula, data, na.action, \dots)

\method{model.frame}{lm}(formula, data, na.action, \dots)
}
\arguments{
  \item{formula}{a model formula}

  \item{data}{\code{data.frame}, list, \code{environment} or object
    coercible to \code{data.frame} containing the variables in
    \code{formula}.}

  \item{subset}{a specification of the rows to be used: defaults to all
    rows. This can be any valid indexing vector (see
    \code{\link{[.data.frame}} for the rows of \code{data} or if that is not
    supplied, a data frame made up of the variables used in \code{formula}.}

  \item{na.action}{how \code{NA}s are treated. The default is first,
    any \code{na.action} attribute of \code{data}, second
    a \code{na.action} setting of \code{\link{options}}, and third
    \code{\link{na.fail}} if that is unset.  The \dQuote{factory-fresh}
    default is \code{\link{na.omit}}.}

  \item{drop.unused.levels}{should factors have unused levels dropped?
    Defaults to \code{FALSE}.}

  \item{xlev}{a named list of character vectors giving the full set of levels
    to be assumed for each factor.}

  \item{\dots}{further arguments such as \code{subset}, \code{offset} and
    \code{weights}. \code{NULL} arguments are treated as missing.}
}
\description{
  \code{model.frame} (a generic function) and its methods return a
  \code{\link{data.frame}} with the variables needed to use
  \code{formula} and any \code{\dots} arguments.
}
\details{
  Variables in the formula, \code{subset} and in \code{\dots} are looked
  for first in \code{data} and then in the environment of
  \code{formula}: see the help for \code{\link{formula}()} for further
  details.

  First all the variables needed are collected into a data frame.
  Then \code{subset}
  expression is evaluated, and it is is used as a row index to the data
  frame. Then the \code{na.action} function is applied to the data frame
  (and may well add attributes).  The levels of any factors in the data
  frame are adjusted according to the \code{drop.unused.levels} and
  \code{xlev} arguments.
}
\value{
  A \code{\link{data.frame}} containing the variables used in
  \code{formula} plus those specified \code{\dots}.
}
\seealso{\code{\link{model.matrix}} for the \dQuote{design matrix},
  \code{\link{formula}} for formulas  and
  \code{\link{expand.model.frame}} for model.frame manipulation.
}
\references{
  Chambers, J. M. (1992)
  \emph{Data for models.}
  Chapter 3 of \emph{Statistical Models in S}
  eds J. M. Chambers and T. J. Hastie, Wadsworth \& Brooks/Cole.
}
\examples{
data(cars)
data.class(model.frame(dist ~ speed, data = cars))
}
\keyword{models}
