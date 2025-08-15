% File src/library/datasets/man/Loblolly.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2011 R Core Team
% Distributed under GPL 2 or later

\name{Loblolly}
\docType{data}
\alias{Loblolly}
\title{Growth of \I{Loblolly} Pine Trees}
\description{
  The \code{Loblolly} data frame has 84 rows and 3 columns of records of
  the growth of \I{Loblolly} pine trees.
}
\usage{Loblolly}
\format{
  An object of class
  \code{c("nfnGroupedData", "nfGroupedData", "groupedData", "data.frame")}
  containing the following columns:
  \describe{
    \item{\code{height}}{
      a numeric vector of tree heights (ft).
    }
    \item{\code{age}}{
      a numeric vector of tree ages (yr).
    }
    \item{\code{Seed}}{
      an ordered factor indicating the seed source for the tree.
      The ordering is according to increasing maximum height.
    }
  }
}
\details{
  This dataset was originally part of package \CRANpkg{nlme}, and that has
  methods (including for \code{[}, \code{as.data.frame}, \code{plot} and
  \code{print}) for its grouped-data classes.
}
\source{
  \bibshow{R:Kung:1986,
    R:Pinheiro+Bates:2000}
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
