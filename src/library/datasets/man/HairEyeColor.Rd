% File src/library/datasets/man/HairEyeColor.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
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
    1 \tab Hair \tab Black, Brown, Red, Blond \cr
    2 \tab Eye \tab Brown, Blue, Hazel, Green \cr
    3 \tab Sex \tab Male, Female
  }
}
\details{
  The Hair \eqn{\times}{x} Eye table comes rom a survey of students at
  the University of Delaware reported by Snee (1974).  The split by
  \code{Sex} was added by Friendly (1992a) for didactic purposes.

  This data set is useful for illustrating various techniques for the
  analysis of contingency tables, such as the standard chi-squared test
  or, more generally, log-linear modelling, and graphical methods such
  as mosaic plots, sieve diagrams or association plots.
}
\source{
  \url{http://euclid.psych.yorku.ca/ftp/sas/vcd/catdata/haireye.sas}

  Snee (1974) gives the two-way table aggregated over \code{Sex}.  The
  \code{Sex} split of the \sQuote{Brown hair, Brown eye} cell was
  changed to agree with that used by Friendly (2000).
}
\references{
  Snee, R. D. (1974)
  Graphical display of two-way contingency tables.
  \emph{The American Statistician}, \bold{28}, 9--12.

  Friendly, M. (1992a)
  Graphical methods for categorical data.
  \emph{SAS User Group International Conference Proceedings}, \bold{17},
  190--200.
  \url{http://www.math.yorku.ca/SCS/sugi/sugi17-paper.html}

  Friendly, M. (1992b)
  Mosaic displays for loglinear models.
  \emph{Proceedings of the Statistical Graphics Section},
  American Statistical Association, pp. 61--68.
  \url{http://www.math.yorku.ca/SCS/Papers/asa92.html}

  Friendly, M. (2000)
  \emph{Visualizing Categorical Data.}
  SAS Institute, ISBN 1-58025-660-0.
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
