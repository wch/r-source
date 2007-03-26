\name{graphics-defunct}
\alias{graphics-defunct}
\alias{dotplot}
\alias{stripplot}
\alias{piechart}
\title{Defunct Functions in Package graphics}
\description{
  The functions or variables listed here are no longer part of \R as
  they are not needed (any more).
}
\usage{
% Removed in 1.3.0
dotplot(\dots)
stripplot(\dots)
% </Removed 1.3.0>
% Removed in 1.6.0
piechart(x, labels = names(x), edges = 200, radius = 0.8,
         density = NULL, angle = 45, col = NULL, main = NULL, \dots)
% </Removed 1.6.0>
}
\details{
  \code{dotplot} and \code{stripplot} have been renamed to
  \code{\link{dotchart}} and \code{\link{stripchart}},
  respectively.  The names are now used in \pkg{lattice}.

  \code{piechart} is the old name for \code{pie}, but clashed with usage
  in Trellis.
}
\seealso{
  \code{\link{Defunct}}
}
\keyword{internal}
