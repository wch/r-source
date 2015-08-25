% File src/library/stats/man/NLSstLfAsymptote.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2010 R Core Team
% Distributed under GPL 2 or later

\name{NLSstLfAsymptote}
\encoding{UTF-8}
\title{Horizontal Asymptote on the Left Side}
\alias{NLSstLfAsymptote}
\alias{NLSstLfAsymptote.sortedXyData}
\description{
  Provide an initial guess at the horizontal asymptote on the left side
  (i.e., small values of \code{x}) of the graph of \code{y} versus
  \code{x} from the \code{xy} object.  Primarily used within
  \code{initial} functions for self-starting nonlinear regression
  models.
}
\usage{
NLSstLfAsymptote(xy)
}
\arguments{
 \item{xy}{a \code{sortedXyData} object}
}
\value{
  A single numeric value estimating the horizontal asymptote for small
  \code{x}.
}
\author{\enc{José}{Jose} Pinheiro and Douglas Bates}
\seealso{
  \code{\link{sortedXyData}},
  \code{\link{NLSstClosestX}},
  \code{\link{NLSstRtAsymptote}},
  \code{\link{selfStart}}
}
\examples{
DNase.2 <- DNase[ DNase$Run == "2", ]
DN.srt <- sortedXyData( expression(log(conc)), expression(density), DNase.2 )
NLSstLfAsymptote( DN.srt )
}
\keyword{manip}
