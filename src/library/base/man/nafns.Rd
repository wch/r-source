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
  \item{omit}{
    An object produced by an \code{na.action} function.
  }
  \item{x}{
    A vector, data frame, or matrix to be adjusted based upon the
    missing value information.
  }
  \item{\dots}{further arguments passed to or from other methods.}
}
\value{
  These return a similar object to \code{x}.
}
\details{
  These are utility functions used to allow \code{\link{predict}} and
  \code{\link{resid}} methods for modelling functions to compensate for
  the removal of \code{NA}s in the fitting process.  There are used by
  the default, \code{"lm"} and \code{"glm"} methods, and by further
  methods in packages \code{MASS}, \code{rpart} and \code{survival}.

  The default methods do nothing.  The method for the \code{na.exclude}
  action to pad the object with \code{NA}s in the correct positions to
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
