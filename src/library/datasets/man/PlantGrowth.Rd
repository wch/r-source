% File src/library/datasets/man/PlantGrowth.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{PlantGrowth}
\docType{data}
\alias{PlantGrowth}
\title{Results from an Experiment on Plant Growth}
\description{
  Results from an experiment to compare yields (as measured by dried
  weight of plants) obtained under a control and two different treatment
  conditions.
}
\usage{PlantGrowth}
\format{
  A data frame of 30 cases on 2 variables.

  \tabular{rll}{
    [, 1] \tab weight \tab numeric \cr
    [, 2] \tab group  \tab factor
  }

  The levels of \code{group} are \sQuote{ctrl}, \sQuote{trt1}, and \sQuote{trt2}.
}
\examples{
## One factor ANOVA example from Dobson's book, cf. Table 7.4:
require(stats); require(graphics)
boxplot(weight ~ group, data = PlantGrowth, main = "PlantGrowth data",
        ylab = "Dried weight of plants", col = "lightgray",
        notch = TRUE, varwidth = TRUE)
anova(lm(weight ~ group, data = PlantGrowth))
}
\source{
  Dobson, A. J. (1983)
  \emph{An Introduction to Statistical Modelling}.
  London: Chapman and Hall.
}
\keyword{datasets}
