\name{naresid}
\alias{naresid}
\alias{naresid.default}
\alias{naresid.exclude}
\alias{napredict}
\alias{napredict.default}
\alias{napredict.exclude}
\title{
Adjust for Missing Values
}
\description{
  Use missing value information to adjust residuals and predictions.
}
\usage{
naresid(omit, x, \dots)
napredict(omit, x, \dots)
}
\arguments{
  \item{omit}{an object produced by an \code{\link{na.action}} function,
    typically the \code{"na.action"} attribute of the result of
    \code{\link{na.omit}} or \code{\link{na.exclude}}.}
  \item{x}{a vector, data frame, or matrix to be adjusted based upon the
    missing value information.}
  \item{\dots}{further arguments passed to or from other methods.}
}
\value{
  These return a similar object to \code{x}.
}
\details{
  These are utility functions used to allow \code{\link{predict}},
  \code{\link{fitted}} and \code{\link{residuals}} methods for modelling
  functions to compensate for the removal of \code{NA}s in the fitting
  process.  They are used by the default, \code{"lm"}, \code{"glm"} and
  \code{"nls"} methods, and by further methods in packages \pkg{MASS},
  \pkg{rpart} and \pkg{survival}.  Also used for the scores returned by
  \code{\link{factanal}}, \code{\link{prcomp}} and \code{\link{princomp}}.

  The default methods do nothing.  The default method for the \code{na.exclude}
  action is to pad the object with \code{NA}s in the correct positions to
  have the same number of rows as the original data frame.

  Currently \code{naresid} and \code{napredict} are identical, but
  future methods need not be.  \code{naresid} is used for residuals, and
  \code{napredict} for fitted values and predictions.
}
\note{
  Packages \code{rpart} and \code{survival5} used to contain versions of
  these functions that had an \code{na.omit} action equivalent to that
  now used for \code{na.exclude}.
}
\keyword{NA}
\keyword{models}
