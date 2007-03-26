\name{terms}
\title{Model Terms}
\usage{
terms(x, \dots)
}
\alias{terms}
\alias{terms.terms}
\alias{terms.aovlist}
\alias{terms.default}
\alias{print.terms}
\alias{labels.terms}
\description{
  The function \code{terms} is a generic function
  which can be used to extract \emph{terms} objects
  from various kinds of \R data objects.
}
\arguments{
  \item{x}{object used to select a method to dispatch.}
  \item{\dots}{further arguments passed to or from other methods.}
}
\details{
  There are methods for classes \code{"aovlist"}, and \code{"terms"}
  \code{"formula"} (see \code{\link{terms.formula}}):
  the default method just extracts the \code{terms} component of the
  object (if any).

  There are \code{\link{print}} and \code{\link{labels}} methods for
  class \code{"terms"}: the latter prints the term labels (see
  \code{\link{terms.object}}).
}
\value{
  An object of class \code{c("terms", "formula")} which contains the
  \emph{terms} representation of a symbolic model. See
  \code{\link{terms.object}} for its structure.
}
\references{
  Chambers, J. M. and Hastie, T. J. (1992)
  \emph{Statistical models.}
  Chapter 2 of \emph{Statistical Models in S}
  eds J. M. Chambers and T. J. Hastie, Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{terms.object}}, \code{\link{terms.formula}},
  \code{\link{lm}}, \code{\link{glm}}, \code{\link{formula}}.
}
%%--- MM: I would really like instructive examples here...
\keyword{models}
