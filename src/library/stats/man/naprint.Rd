\name{naprint}
\alias{naprint}
\alias{naprint.default}
\alias{naprint.exclude}
\alias{naprint.omit}
\title{
Adjust for Missing Values
}
\description{
  Use missing value information to report the effects of an \code{na.action}.
}
\usage{
naprint(x, \dots)
}
\arguments{
  \item{x}{
    An object produced by an \code{na.action} function.
  }
  \item{\dots}{further arguments passed to or from other methods.}
}
\details{
  This is a generic function, and the exact information differs by
  method. \code{naprint.omit} reports the number of rows omitted:
  \code{naprint.default} reports an empty string.
}
\value{
  A character string providing information on missing values, for
  example the number.
}
\keyword{NA}
\keyword{models}
