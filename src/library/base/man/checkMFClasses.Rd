\name{.checkMFClasses}
\alias{.checkMFClasses}
\alias{.MFclass}
\title{Functions to Check the Type of Variables passed to Model Frames}
\description{
  \code{.checkMFClasses} checks if the variables used in a predict
  method agree in type with those used for fitting.

  \code{.MFclass} categories variables for this purpose.
}
\usage{
.checkMFClasses(cl, m)
.MFclass(x)
}
\arguments{
  \item{cl}{a character vector of class descriptions to match.}
  \item{m}{a model frame}
  \item{x}{any \R object}
}
\value{
  \code{.MFclass} returns a character string, one of \code{"logical"},
  \code{"ordered"}, \code{"factor"}, \code{"numeric"}, \code{"nmatrix.*"}
  (a numeric matrix with a number of columns appended) or \code{"other"}.
}
\keyword{internal}
