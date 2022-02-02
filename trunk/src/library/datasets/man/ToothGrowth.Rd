% File src/library/datasets/man/ToothGrowth.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2018 R Core Team
% Distributed under GPL 2 or later

\name{ToothGrowth}
\docType{data}
\alias{ToothGrowth}
\title{The Effect of Vitamin C on Tooth Growth in Guinea Pigs}
\description{
  The response is the length of odontoblasts (cells responsible for
  tooth growth) in 60 guinea pigs.  Each animal received one of three
  dose levels of vitamin C (0.5, 1, and 2 mg/day) by one of two delivery
  methods, orange juice or ascorbic acid (a form of vitamin C and coded
  as \code{VC}).
}
\usage{ToothGrowth}
\format{
  A data frame with 60 observations on 3 variables.
  \tabular{rlll}{
    [,1]  \tab len   \tab numeric  \tab Tooth length\cr
    [,2]  \tab supp  \tab factor   \tab Supplement type (VC or OJ).\cr
    [,3]  \tab dose  \tab numeric  \tab Dose in milligrams/day
  }
}
\source{
  C. I. Bliss (1952).
  \emph{The Statistics of Bioassay}.
  Academic Press.
}
\references{
  McNeil, D. R. (1977).
  \emph{Interactive Data Analysis}.
  New York: Wiley.

  Crampton, E. W. (1947).
  The growth of the odontoblast of the incisor teeth as a criterion of
  vitamin C intake of the guinea pig.
  \emph{The Journal of Nutrition}, \bold{33}(5), 491--504.
  \doi{10.1093/jn/33.5.491}.
}
\examples{
require(graphics)
coplot(len ~ dose | supp, data = ToothGrowth, panel = panel.smooth,
       xlab = "ToothGrowth data: length vs dose, given type of supplement")
}
\keyword{datasets}
