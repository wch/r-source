\name{CO2}
\docType{data}
\alias{CO2}
\title{Carbon Dioxide uptake in grass plants}
\description{
  The \code{CO2} data frame has 84 rows and 5 columns of data from an
  experiment on the cold tolerance of the grass species
  \emph{Echinochloa crus-galli}.
}
\usage{CO2}
\format{
  This data frame contains the following columns:
  \describe{
    \item{Plant}{
      an ordered factor with levels
      \code{Qn1} < \code{Qn2} < \code{Qn3} < \dots < \code{Mc1}
      giving a unique identifier for each plant.
    }
    \item{Type}{
      a factor with levels
      \code{Quebec} 
      \code{Mississippi}
      giving the origin of the plant
    }
    \item{Treatment}{
      a factor with levels
      \code{nonchilled} 
      \code{chilled}
    }
    \item{conc}{
      a numeric vector of ambient carbon dioxide concentrations (mL/L).
    }
    \item{uptake}{
      a numeric vector of carbon dioxide uptake rates
      (\eqn{\mu\mbox{mol}/m^2}{umol/m^2} sec).
    }
  }
}
\details{
  The \eqn{CO_2}{CO2} uptake of six plants from Quebec and six plants
  from Mississippi was measured at several levels of ambient
  \eqn{CO_2}{CO2} concentration.  Half the plants of each type were
  chilled overnight before the experiment was conducted.
}
\source{
  Potvin, C., Lechowicz, M. J. and Tardif, S. (1990)
  \dQuote{The statistical analysis of ecophysiological response curves
  obtained from experiments involving repeated measures}, \emph{Ecology},
  \bold{71}, 1389--1400.

  Pinheiro, J. C. and Bates, D. M. (2000)
  \emph{Mixed-effects Models in S and S-PLUS}, Springer.
}
\examples{
require(stats); require(graphics)
coplot(uptake ~ conc | Plant, data = CO2, show.given = FALSE, type = "b")
## fit the data for the first plant
fm1 <- nls(uptake ~ SSasymp(conc, Asym, lrc, c0),
   data = CO2, subset = Plant == 'Qn1')
summary(fm1)
## fit each plant separately
fmlist <- list()
for (pp in levels(CO2$Plant)) {
  fmlist[[pp]] <- nls(uptake ~ SSasymp(conc, Asym, lrc, c0),
      data = CO2, subset = Plant == pp)
}
## check the coefficients by plant
sapply(fmlist, coef)
}
\keyword{datasets}
