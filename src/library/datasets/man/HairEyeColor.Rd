% File src/library/datasets/man/HairEyeColor.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2025 R Core Team
% Distributed under GPL 2 or later

\name{HairEyeColor}
\docType{data}
\alias{HairEyeColor}
\title{Hair and Eye Color of Statistics Students}
\description{
  Distribution of hair and eye color and sex in 592 statistics students.
}
\usage{HairEyeColor}
\format{
  A 3-dimensional array resulting from cross-tabulating 592 observations
  on 3 variables.  The variables and their levels are as follows:

  \tabular{rll}{
    No \tab Name \tab Levels \cr
    1 \tab \code{Hair} \tab Black, Brown, Red, Blond \cr
    2 \tab \code{Eye}  \tab Brown, Blue, Hazel, Green \cr
    3 \tab \code{Sex}  \tab Male, Female
  }
}
\details{
  The Hair \eqn{\times}{x} Eye table comes from a survey of students at
  the University of Delaware reported by \bibcitet{R:Snee:1974}.  The split by
  \code{Sex} was added by \bibcitet{R:Friendly:1992a} for didactic purposes.

  This data set is useful for illustrating various techniques for the
  analysis of contingency tables, such as the standard chi-squared test
  or, more generally, log-linear modelling, and graphical methods such
  as mosaic plots, sieve diagrams or association plots.
}
\source{
  \url{http://www.datavis.ca/sas/vcd/catdata/haireye.sas}

  \bibcitet{R:Snee:1974} gives the two-way table aggregated over \code{Sex}.  The
  \code{Sex} split of the \sQuote{Brown hair, Brown eye} cell was
  changed to agree with that used by \bibcitet{R:Friendly:2000}.
}
\references{
  \bibshow{*, R:Friendly:1992b}
}
\seealso{
  \code{\link{chisq.test}},
  \code{\link{loglin}},
  \code{\link{mosaicplot}}
}
\examples{
require(graphics)
## Full mosaic
mosaicplot(HairEyeColor)
## Aggregate over sex (as in Snee's original data)
x <- apply(HairEyeColor, c(1, 2), sum)
x
mosaicplot(x, main = "Relation between hair and eye color")
}
\keyword{datasets}
