% File src/library/stats/man/pairwise.t.test.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{pairwise.t.test}
\alias{pairwise.t.test}
\title{ Pairwise t tests}
\description{
 Calculate pairwise comparisons between group levels with corrections
 for multiple testing
}
\usage{
pairwise.t.test(x, g, p.adjust.method = p.adjust.methods,
                pool.sd = !paired, paired = FALSE,
                alternative = c("two.sided", "less", "greater"),
                \dots)
}
\arguments{
 \item{x}{ response vector. }
 \item{g}{ grouping vector or factor. }
 \item{p.adjust.method}{ Method for adjusting p values (see \code{\link{p.adjust}}). }
 \item{pool.sd}{ switch to allow/disallow the use of a pooled SD }
 \item{paired}{ a logical indicating whether you want paired
    t-tests. }
 \item{alternative}{ a character string specifying the alternative
    hypothesis, must be one of \code{"two.sided"} (default),
    \code{"greater"} or \code{"less"}.  }
 \item{\dots}{ additional arguments to pass to \code{t.test}. }
}
\details{ The \code{pool.sd} switch calculates a common SD for all
  groups and uses that for all comparisons (this can be useful if some
  groups are small). This method does not actually call \code{t.test},
  so extra arguments are ignored. Pooling does not generalize to paired tests
  so \code{pool.sd} and \code{paired} cannot both be \code{TRUE}.

  Only the lower triangle of the matrix of possible comparisons is being
  calculated, so setting \code{alternative} to anything other than
  \code{"two.sided"} requires that the levels of \code{g} are ordered
  sensibly.
}
\value{
  Object of class \code{"pairwise.htest"}
}
\seealso{ \code{\link{t.test}}, \code{\link{p.adjust}}}

\examples{
attach(airquality)
Month <- factor(Month, labels = month.abb[5:9])
pairwise.t.test(Ozone, Month)
pairwise.t.test(Ozone, Month, p.adj = "bonf")
pairwise.t.test(Ozone, Month, pool.sd = FALSE)
detach()
}
\keyword{htest}
