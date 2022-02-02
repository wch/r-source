% File src/library/stats/man/checkMFClasses.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2019 R Core Team
% Distributed under GPL 2 or later

\name{.checkMFClasses}
\alias{.checkMFClasses}
\alias{.MFclass}
\alias{.getXlevels}
\title{Functions to Check the Type of Variables passed to Model Frames}
\description{
  \code{.checkMFClasses} checks if the variables used in a predict
  method agree in type with those used for fitting.

  \code{.MFclass} categorizes variables for this purpose.

  \code{.getXlevels()} extracts factor levels from \code{\link{factor}} or
  \code{\link{character}} variables.
}
\usage{
.checkMFClasses(cl, m, ordNotOK = FALSE)
.MFclass(x)
.getXlevels(Terms, m)
}
\arguments{
  \item{cl}{a character vector of class descriptions to match.}
  \item{m}{a model frame (\code{\link{model.frame}()} result).}
  \item{x}{any \R object.}
  \item{ordNotOK}{logical: are ordered factors different?}
  \item{Terms}{a \code{terms} object (\code{\link{terms.object}}).}
}
\details{
  For applications involving \code{\link{model.matrix}()} such as linear models
  we do not need to differentiate between \emph{ordered} factors and factors as
  although these affect the coding, the coding used in the fit is
  already recorded and imposed during prediction.  However, other
  applications may treat ordered factors differently:
  \code{\link[rpart]{rpart}} does, for example.
}
\value{
  \code{.checkMFClasses()} checks and either signals an error calling
  \code{\link{stop}()} or returns \code{\link{NULL}} invisibly.

  \code{.MFclass()} returns a character string, one of \code{"logical"},
  \code{"ordered"}, \code{"factor"}, \code{"numeric"}, \code{"nmatrix.*"}
  (a numeric matrix with a number of columns appended) or \code{"other"}.

  \code{.getXlevels} returns a named \code{\link{list}} of character
  vectors, possibly empty, or \code{\link{NULL}}.
}
\examples{
sapply(warpbreaks, .MFclass) # "numeric" plus 2 x "factor"
sapply(iris,       .MFclass) # 4 x "numeric" plus "factor"

mf <- model.frame(Sepal.Width ~ Species,      iris)
mc <- model.frame(Sepal.Width ~ Sepal.Length, iris)

.checkMFClasses("numeric", mc) # nothing else
.checkMFClasses(c("numeric", "factor"), mf)

## simple .getXlevels() cases :
(xl <- .getXlevels(terms(mf), mf)) # a list with one entry " $ Species" with 3 levels:
stopifnot(exprs = {
  identical(xl$Species, levels(iris$Species))
  identical(.getXlevels(terms(mc), mc), xl[0]) # a empty named list, as no factors
  is.null(.getXlevels(terms(x~x), list(x=1)))
})
}
\keyword{utilities}
