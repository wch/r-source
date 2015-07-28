% File src/library/stats/man/pairwise.wilcox.test.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{pairwise.wilcox.test}
\alias{pairwise.wilcox.test}
\title{Pairwise Wilcoxon Rank Sum Tests}
\description{
  Calculate pairwise comparisons between group levels with corrections
  for multiple testing.
}
\usage{
pairwise.wilcox.test(x, g, p.adjust.method = p.adjust.methods,
                      paired = FALSE, \dots)
}
\arguments{
  \item{x}{ response vector. }
  \item{g}{ grouping vector or factor. }
  \item{p.adjust.method}{ method for adjusting p values (see
    \code{\link{p.adjust}}). Can be abbreviated.}
  \item{paired}{a logical indicating whether you want a paired test.}
  \item{\dots}{additional arguments to pass to \code{\link{wilcox.test}}.}
}
\details{
  Extra arguments that are passed on to \code{wilcox.test} may or may
  not be sensible in this context. In particular,
  only the lower triangle of the matrix of possible comparisons is being
  calculated, so setting \code{alternative} to anything other than
  \code{"two.sided"} requires that the levels of \code{g} are ordered
  sensibly.
}
\value{
  Object of class \code{"pairwise.htest"}
}
\seealso{ \code{\link{wilcox.test}}, \code{\link{p.adjust}}}

\examples{
attach(airquality)
Month <- factor(Month, labels = month.abb[5:9])
## These give warnings because of ties :
pairwise.wilcox.test(Ozone, Month)
pairwise.wilcox.test(Ozone, Month, p.adj = "bonf")
detach()
}
\keyword{htest}
