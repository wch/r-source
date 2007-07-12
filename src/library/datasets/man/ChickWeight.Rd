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
  This data frame contains the following columns:
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
      a factor with levels 1,\dots,4 indicating which
      experimental diet the chick received.
    }
  }
}
\details{
  The body weights of the chicks were measured at birth and every
  second day thereafter until day 20.  They were also measured on day
  21.  There were four groups on chicks on different protein diets.
}
\source{
  Crowder, M. and Hand, D. (1990), \emph{Analysis of Repeated Measures},
  {Chapman and Hall} (example 5.3)

  Hand, D. and Crowder, M. (1996), \emph{Practical Longitudinal Data
    Analysis}, Chapman and Hall (table A.2)

  Pinheiro, J. C. and Bates, D. M. (2000) \emph{Mixed-effects Models in
    S and S-PLUS}, Springer.
}
\examples{
require(stats); require(graphics)
coplot(weight ~ Time | Chick, data = ChickWeight,
    type = "b", show.given = FALSE)
## fit a representative chick
fm1 <- nls(weight ~ SSlogis( Time, Asym, xmid, scal ),
    data = ChickWeight, subset = Chick == 1)
summary( fm1 )
}
\keyword{datasets}
