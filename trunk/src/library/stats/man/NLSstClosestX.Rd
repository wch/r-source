% File src/library/stats/man/NLSstClosestX.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2010 R Core Team
% Distributed under GPL 2 or later

\name{NLSstClosestX}
\encoding{UTF-8}
\title{Inverse Interpolation}
\alias{NLSstClosestX}
\alias{NLSstClosestX.sortedXyData}
\usage{
NLSstClosestX(xy, yval)
}
\arguments{
 \item{xy}{a \code{sortedXyData} object}
 \item{yval}{a numeric value on the \code{y} scale}
}
\description{
  Use inverse linear interpolation to approximate the \code{x} value at
  which the function represented by \code{xy} is equal to \code{yval}.
}
\value{
  A single numeric value on the \code{x} scale.
}
\author{\enc{José}{Jose} Pinheiro and Douglas Bates}
\seealso{\code{\link{sortedXyData}}, \code{\link{NLSstLfAsymptote}},
  \code{\link{NLSstRtAsymptote}}, \code{\link{selfStart}}}
\examples{
DNase.2 <- DNase[ DNase$Run == "2", ]
DN.srt <- sortedXyData(expression(log(conc)), expression(density), DNase.2)
NLSstClosestX(DN.srt, 1.0)
}
\keyword{manip}
