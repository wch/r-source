% File src/library/stats/man/pairwise.prop.test.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{pairwise.prop.test}
\alias{pairwise.prop.test}
\title{ Pairwise comparisons for proportions}
\description{
  Calculate pairwise comparisons between pairs of proportions with
  correction for multiple testing
}
\usage{
pairwise.prop.test(x, n, p.adjust.method = p.adjust.methods, ...)
}
\arguments{
 \item{x}{ Vector of counts of successes or a matrix with 2 columns
   giving the counts of successes and failures, respectively. }
 \item{n}{ Vector of counts of trials; ignored if \code{x} is a matrix.}
 \item{p.adjust.method}{Method for adjusting p values
   (see \code{\link{p.adjust}}) }
 \item{\dots}{ Additional arguments to pass to \code{prop.test} }
}
\value{
  Object of class \code{"pairwise.htest"}
}
\seealso{ \code{\link{prop.test}}, \code{\link{p.adjust}}}
\examples{
smokers  <- c( 83, 90, 129, 70 )
patients <- c( 86, 93, 136, 82 )
pairwise.prop.test(smokers, patients)
}
\keyword{htest}
