% File src/library/datasets/man/Loblolly.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2011 R Core Team
% Distributed under GPL 2 or later

\name{Loblolly}
\docType{data}
\alias{Loblolly}
\title{Growth of Loblolly pine trees}
\description{
  The \code{Loblolly} data frame has 84 rows and 3 columns of records of
  the growth of Loblolly pine trees.
}
\usage{Loblolly}
\format{
  An object of class
  \code{c("nfnGroupedData", "nfGroupedData", "groupedData", "data.frame")}
  containing the following columns:
  \describe{
    \item{height}{
      a numeric vector of tree heights (ft).
    }
    \item{age}{
      a numeric vector of tree ages (yr).
    }
    \item{Seed}{
      an ordered factor indicating the seed source for the tree.
      The ordering is according to increasing maximum height.
    }
  }
}
\details{
  This dataset was originally part of package \code{nlme}, and that has
  methods (including for \code{[}, \code{as.data.frame}, \code{plot} and
  \code{print}) for its grouped-data classes.
}
\source{
  Kung, F. H. (1986),
  Fitting logistic growth curve with predetermined carrying capacity,
  in \emph{Proceedings of the Statistical Computing Section,
    American Statistical Association}, 340--343.

  Pinheiro, J. C. and Bates, D. M. (2000)
  \emph{Mixed-effects Models in S and S-PLUS}, Springer.
}
\examples{
require(stats); require(graphics)
plot(height ~ age, data = Loblolly, subset = Seed == 329,
     xlab = "Tree age (yr)", las = 1,
     ylab = "Tree height (ft)",
     main = "Loblolly data and fitted curve (Seed 329 only)")
fm1 <- nls(height ~ SSasymp(age, Asym, R0, lrc),
           data = Loblolly, subset = Seed == 329)
age <- seq(0, 30, length.out = 101)
lines(age, predict(fm1, list(age = age)))
}
\keyword{datasets}
