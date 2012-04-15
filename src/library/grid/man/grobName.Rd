% File src/library/grid/man/grobName.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{grobName}
\alias{grobName}
\title{Generate a Name for a Grob}
\description{
  This function generates a unique (within-session) name for
  a grob, based on the grob's class.
}
\usage{
grobName(grob = NULL, prefix = "GRID")
}
\arguments{
  \item{grob}{A grob object or \code{NULL}.}
  \item{prefix}{The prefix part of the name.}
}
\value{
  A character string of the form \code{prefix.class(grob).index}
}
\author{Paul Murrell}
\keyword{dplot}
