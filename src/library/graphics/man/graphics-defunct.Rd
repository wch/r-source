% File src/library/graphics/man/graphics-defunct.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\newcommand{\CRANpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}

\name{graphics-defunct}
\alias{graphics-defunct}
\alias{dotplot}
\alias{stripplot}
\alias{piechart}
\title{Defunct Functions in Package \pkg{graphics}}
\description{
  The functions or variables listed here are no longer part of \R as
  they are not needed (any more).
}
\usage{
# Defunct in 1.3.0
dotplot(\dots)
stripplot(\dots)

# Defunct in 1.6.0
piechart(x, labels = names(x), edges = 200, radius = 0.8,
         density = NULL, angle = 45, col = NULL, main = NULL, \dots)
}
\details{
  \code{dotplot} and \code{stripplot} have been renamed to
  \code{\link{dotchart}} and \code{\link{stripchart}},
  respectively.  The names are now used in \CRANpkg{lattice}.

  \code{piechart} was the old name for \code{pie}, but clashed with usage
  in Trellis (not implemented in \CRANpkg{lattice}).
}
\seealso{
  \code{\link{Defunct}}
}
\keyword{internal}
