% File src/library/stats/man/TukeyHSD.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{TukeyHSD}
\alias{TukeyHSD}
%\alias{TukeyHSD.aov}
%\alias{print.TukeyHSD}
%\alias{plot.TukeyHSD}
\title{Compute Tukey Honest Significant Differences}
\description{
  Create a set of confidence intervals on the differences between the
  means of the levels of a factor with the specified family-wise
  probability of coverage.  The intervals are based on the Studentized
  range statistic, Tukey's \sQuote{Honest Significant Difference}
  method.
}
\usage{
TukeyHSD(x, which, ordered = FALSE, conf.level = 0.95, \dots)
}
\arguments{
 \item{x}{A fitted model object, usually an \code{\link{aov}} fit.}
 \item{which}{A character vector listing terms in the fitted model for
   which the intervals should be calculated.  Defaults to all the
   terms.}
  \item{ordered}{A logical value indicating if the levels of the factor
   should be ordered according to increasing average in the sample
   before taking differences.  If \code{ordered} is true then
   the calculated differences in the means will all be positive.  The
   significant differences will be those for which the \code{lwr} end
   point is positive.}
 \item{conf.level}{A numeric value between zero and one giving the
   family-wise confidence level to use.}
 \item{\dots}{Optional additional arguments.  None are used at present.}
}
\details{
  When comparing the means for the levels of a factor in an analysis of
  variance, a simple comparison using t-tests will inflate the
  probability of declaring a significant difference when it is not in
  fact present.  This because the intervals are calculated with a
  given coverage probability for each interval but the interpretation of
  the coverage is usually with respect to the entire family of
  intervals.

  John Tukey introduced intervals based on the range of the
  sample means rather than the individual differences.  The intervals
  returned by this function are based on this Studentized range
  statistics.

  The intervals constructed in this way would only apply exactly to
  balanced designs where there are the same number of observations made
  at each level of the factor.  This function incorporates an adjustment
  for sample size that produces sensible intervals for mildly unbalanced
  designs.

  If \code{which} specifies non-factor terms these will be dropped with
  a warning: if no terms are left this is a an error.

  In \R 2.15.2 and earlier the results were \code{NA} for fits with
  \code{na.action} methods such as \code{\link{na.exclude}}.
}
\value{
  A list of class \code{c("multicomp", "TukeyHSD")},
  with one component for each term requested in \code{which}.
  Each component is a matrix with columns \code{diff} giving the
  difference in the observed means, \code{lwr} giving the lower
  end point of the interval, \code{upr} giving the upper end point
  and \code{p adj} giving the p-value after adjustment for the multiple
  comparisons.

  There are \code{print} and \code{plot} methods for class
  \code{"TukeyHSD"}.  The \code{plot} method does not accept
  \code{xlab}, \code{ylab} or \code{main} arguments and creates its own
  values for each plot.
}
\references{
  Miller, R. G. (1981)
  \emph{Simultaneous Statistical Inference}. Springer.

  Yandell, B. S. (1997)
  \emph{Practical Data Analysis for Designed Experiments}.
  Chapman & Hall.
}
\author{
  Douglas Bates
}
\seealso{
  \code{\link{aov}}, \code{\link{qtukey}}, \code{\link{model.tables}},
  \code{\link[multcomp]{glht}} in package \CRANpkg{multcomp}.
}
\examples{
require(graphics)

summary(fm1 <- aov(breaks ~ wool + tension, data = warpbreaks))
TukeyHSD(fm1, "tension", ordered = TRUE)
plot(TukeyHSD(fm1, "tension"))
}
\keyword{models}
\keyword{design}
