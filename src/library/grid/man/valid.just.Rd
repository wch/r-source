% File src/library/grid/man/valid.just.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{valid.just}
\alias{valid.just}
\title{Validate a Justification}
\description{
  This utility function is useful for determining
  whether a justification specification is valid.
  An error is given if the justification is not valid.
}
\usage{
valid.just(just)
}
\arguments{
  \item{just}{A justification either as a character value,
    e.g., \code{"left"}, or as a numeric value, e.g., 0.}
}
\details{
  This function is useful within a \code{validDetails} method
  when writing a new grob class.
}
\value{
  A numeric representation of the justification (e.g.,
  \code{"left"} becomes 0, \code{"right"} becomes 1, etc, ...).
}
\author{Paul Murrell}
\keyword{dplot}
