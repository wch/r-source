% File src/library/grid/man/valid.just.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{valid.just}
\alias{valid.just}
\alias{resolveHJust}
\alias{resolveVJust}
\title{Validate a Justification}
\description{
  Utility functions for determining
  whether a justification specification is valid
  and for resolving a single justification value from
  a combination of character and numeric values.
}
\usage{
valid.just(just)
resolveHJust(just, hjust) 
resolveVJust(just, vjust) 
}
\arguments{
  \item{just}{A justification either as a character value,
    e.g., \code{"left"}, or as a numeric value, e.g., 0.}
  \item{hjust}{A numeric horizontal justification}
  \item{vjust}{A numeric vertical justification}
}
\details{
  These functions may be useful within a \code{validDetails} method
  when writing a new grob class.
}
\value{
  A numeric representation of the justification (e.g.,
  \code{"left"} becomes 0, \code{"right"} becomes 1, etc, ...).
  An error is given if the justification is not valid.
}
\author{Paul Murrell}
\keyword{dplot}
