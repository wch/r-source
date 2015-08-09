% File src/library/stats/man/plot.density.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{plot.density}
\alias{plot.density}
\title{Plot Method for Kernel Density Estimation}
\usage{
\method{plot}{density}(x, main = NULL, xlab = NULL, ylab = "Density", type = "l",
     zero.line = TRUE, \dots)
}
\arguments{
\item{x}{a \code{"density"} object.}
\item{main, xlab, ylab, type}{plotting parameters with useful defaults.}
\item{\dots}{further plotting parameters.}
\item{zero.line}{logical; if \code{TRUE}, add a base line at \eqn{y = 0}}
}
\description{
  The \code{plot} method for density objects.
}
\value{
  None.
}
\seealso{
  \code{\link{density}}.
}
\keyword{dplot}
