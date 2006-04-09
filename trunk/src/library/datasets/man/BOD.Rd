\name{BOD}
\docType{data}
\alias{BOD}
\title{ Biochemical Oxygen Demand }
\description{
  The \code{BOD} data frame has 6 rows and 2 columns giving the
  biochemical oxygen demand versus time in an evaluation of water
  quality.
}
\usage{BOD}
\format{
  This data frame contains the following columns:
  \describe{
    \item{Time}{
      A numeric vector giving the time of the measurement (days).
    }
    \item{demand}{
      A numeric vector giving the biochemical oxygen demand (mg/l).
    }
  }
}
\source{
  Bates, D.M. and Watts, D.G. (1988),
  \emph{Nonlinear Regression Analysis and Its Applications},
  Wiley, Appendix A1.4.

  Originally from Marske (1967), \emph{Biochemical
    Oxygen Demand Data Interpretation Using Sum of Squares Surface}
  M.Sc. Thesis, University of Wisconsin -- Madison.
}
\examples{
require(stats)
# simplest form of fitting a first-order model to these data
fm1 <- nls(demand ~ A*(1-exp(-exp(lrc)*Time)), data = BOD,
   start = c(A = 20, lrc = log(.35)))
coef(fm1)
print(fm1)
# using the plinear algorithm
fm2 <- nls(demand ~ (1-exp(-exp(lrc)*Time)), data = BOD,
   start = c(lrc = log(.35)), algorithm = "plinear", trace = TRUE)
# using a self-starting model
fm3 <- nls(demand ~ SSasympOrig(Time, A, lrc), data = BOD)
summary( fm3 )
}
\keyword{datasets}
