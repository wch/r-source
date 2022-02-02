% File src/library/stats/man/NLSstRtAsymptote.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2010 R Core Team
% Distributed under GPL 2 or later

\name{NLSstRtAsymptote}
\encoding{UTF-8}
\title{Horizontal Asymptote on the Right Side}
\alias{NLSstRtAsymptote}
\alias{NLSstRtAsymptote.sortedXyData}
\description{
  Provide an initial guess at the horizontal asymptote on the right side
  (i.e., large values of \code{x}) of the graph of \code{y} versus
  \code{x} from the \code{xy} object.  Primarily used within
  \code{initial} functions for self-starting nonlinear regression
  models.
}
\usage{
NLSstRtAsymptote(xy)
}
\arguments{
 \item{xy}{a \code{sortedXyData} object}
}
\value{
  A single numeric value estimating the horizontal asymptote for large
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
NLSstRtAsymptote( DN.srt )
}
\keyword{manip}
