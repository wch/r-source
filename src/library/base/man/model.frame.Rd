\name{model.frame}
\title{Extracting the ``Environment'' of a Model Formula}
\usage{
model.frame(formula, data, na.action, \dots)
model.frame.default(formula, data = sys.frame(sys.parent()), na.action,
                    drop.unused.levels = FALSE, xlev = NULL, \dots)

Methods for
        lm      glm     aovlist
}
\alias{model.frame}
\alias{model.frame.default}
\alias{model.frame.lm}
\alias{model.frame.glm}
\alias{model.frame.aovlist}
\arguments{
  \item{formula}{a model formula}

  \item{data}{\code{data.frame}, list, \code{environment} or object
    coercible to \code{data.frame} containing the variables in
    \code{formula}.}
  
  \item{na.action}{how \code{NA}s are treated. The default is first,
    any \code{na.action} attribute of \code{data}, second
    a \code{na.action} setting of \code{\link{options}}, and third
    \code{\link{na.fail}} if that is unset.  The ``factory-fresh''
    default is \code{\link{na.omit}}.}

  \item{drop.unused.levels}{should factors have unused levels dropped?
    Defaults to \code{FALSE}.}
  
  \item{xlev}{a named list of character vectors giving the full set of levels
    to be assumed for each factor.}
  
  \item{\dots}{further arguments such as \code{subset}, \code{offset} and
    \code{weights}. \code{NULL} arguments are treated as missing.}
}
\description{
  \code{model.frame} (generic function) and its methods return a
  \code{\link{data.frame}} with the variables from \code{formula}.
}
\value{
  A \code{\link{data.frame}} containing the variables used in
  \code{formula}.
}
\seealso{\code{\link{model.matrix}} for the ``design matrix''.}
\examples{
data(cars)
data.class(model.frame(dist ~ speed, data = cars))
}
\keyword{models}
