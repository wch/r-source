\name{naprint}
\alias{naprint}
\alias{naprint.default}
\alias{naprint.exclude}
\alias{naprint.omit}
\title{
Adjust for Missing Values
}
\description{
  Use missing value information to adjust residuals and predictions,
  or to report the effects of an \code{na.action}.
}
\usage{
naprint(omit, x, \dots)
}
\arguments{
\item{omit}{
An object produced by an \code{na.action} function.
}
\item{x}{
  A vector, data frame, or matrix to be adjusted based upon the
  missing value information.
}}
\value{
These return \code{object}.
}
\details{
Currently these functions do nothing. They are provided to enable
other functions to be written in ways compatible with the code in
S-PLUS 2000 which does use these for the \code{na.exclude} action to
pad the object with \code{NA}s in the correct positions to have the same
number of rows as the original data frame.
}
\keyword{NA}
\keyword{models}
