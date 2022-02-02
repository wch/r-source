% File src/library/datasets/man/ChickWeight.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2011 R Core Team
% Distributed under GPL 2 or later

\name{ChickWeight}
\docType{data}
\alias{ChickWeight}
\title{Weight versus age of chicks on different diets}
\description{
  The \code{ChickWeight} data frame has 578 rows and 4 columns from an
  experiment on the effect of diet on early growth of chicks.
}
\usage{ChickWeight}
\format{
  An object of class
  \code{c("nfnGroupedData", "nfGroupedData", "groupedData", "data.frame")}
  containing the following columns:
  \describe{
    \item{weight}{
      a numeric vector giving the body weight of the chick (gm).
    }
    \item{Time}{
      a numeric vector giving the number of days since birth when
      the measurement was made.
    }
    \item{Chick}{
      an ordered factor with levels
      \code{18} < \dots < \code{48}
      giving a unique identifier for the chick.  The ordering of
      the levels groups chicks on the same diet together and
      orders them according to their final weight (lightest to
      heaviest) within diet.
    }
    \item{Diet}{
      a factor with levels 1, \dots, 4 indicating which
      experimental diet the chick received.
    }
  }
}
\details{
  The body weights of the chicks were measured at birth and every
  second day thereafter until day 20.  They were also measured on day
  21.  There were four groups on chicks on different protein diets.

  This dataset was originally part of package \code{nlme}, and that has
  methods (including for \code{[}, \code{as.data.frame}, \code{plot} and
  \code{print}) for its grouped-data classes.
}
\source{
  Crowder, M. and Hand, D. (1990), \emph{Analysis of Repeated Measures},
  Chapman and Hall (example 5.3)

  Hand, D. and Crowder, M. (1996), \emph{Practical Longitudinal Data
    Analysis}, Chapman and Hall (table A.2)

  Pinheiro, J. C. and Bates, D. M. (2000) \emph{Mixed-effects Models in
    S and S-PLUS}, Springer.
}
\seealso{
  \code{\link{SSlogis}} for models fitted to this dataset.
}

\examples{\donttest{
require(graphics)
coplot(weight ~ Time | Chick, data = ChickWeight,
       type = "b", show.given = FALSE)
}}
\keyword{datasets}
