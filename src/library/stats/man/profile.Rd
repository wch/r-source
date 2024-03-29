% File src/library/stats/man/profile.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2023 R Core Team
% Distributed under GPL 2 or later

\name{profile}
\alias{profile}
\title{Generic Function for Profiling Models}
\usage{
profile(fitted, \dots)
}
\arguments{
  \item{fitted}{the original fitted model object.}
  \item{\dots}{additional parameters. See documentation on individual
    methods.}
}
\description{
  Investigates the behavior of the objective function near the solution
  represented by \code{fitted}.

  See documentation on method functions for further details.
}
\value{
  A list with an element for each parameter being profiled.  See the
  individual methods for further details.
}
\seealso{
  \code{\link{profile.nls}}, \code{\link{profile.glm}} \ldots

  \code{\link{plot.profile}}.

  For profiling R code, see \code{\link{Rprof}}.
}
\keyword{models}
