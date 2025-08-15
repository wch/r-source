% File src/library/datasets/man/zCO2.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2011 R Core Team
% Distributed under GPL 2 or later

\name{CO2}
\docType{data}
\alias{CO2}
\title{Carbon Dioxide Uptake in Grass Plants}
\description{
  The \code{CO2} data frame has 84 rows and 5 columns of data from an
  experiment on the cold tolerance of the grass species
  \emph{\I{Echinochloa crus-galli}}.
}
\usage{CO2}
\format{
  An object of class
  \code{c("nfnGroupedData", "nfGroupedData", "groupedData", "data.frame")}
  containing the following columns:
  \describe{
    \item{\code{Plant}}{
      an ordered factor with levels
      \code{Qn1} < \code{Qn2} < \code{Qn3} < \dots < \code{Mc1}
      giving a unique identifier for each plant.
    }
    \item{\code{Type}}{
      a factor with levels
      \code{Quebec}
      \code{Mississippi}
      giving the origin of the plant
    }
    \item{\code{Treatment}}{
      a factor with levels
      \code{nonchilled}
      \code{chilled}
    }
    \item{\code{conc}}{
      a numeric vector of ambient carbon dioxide concentrations (\abbr{mL}/L).
    }
    \item{\code{uptake}}{
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

  This dataset was originally part of package \CRANpkg{nlme}, and that has
  methods (including for \code{[}, \code{as.data.frame}, \code{plot} and
  \code{print}) for its grouped-data classes.
}
\source{
  \bibcitet{R:Potvin+Lechowicz+Tardif:1990,
    R:Pinheiro+Bates:2000}
}
\references{
  \bibshow{*}
}
\examples{
require(stats); require(graphics)
\dontshow{options(show.nls.convergence=FALSE)}
coplot(uptake ~ conc | Plant, data = CO2, show.given = FALSE, type = "b")
## fit the data for the first plant
fm1 <- nls(uptake ~ SSasymp(conc, Asym, lrc, c0),
   data = CO2, subset = Plant == "Qn1")
summary(fm1)
## fit each plant separately
fmlist <- list()
for (pp in levels(CO2$Plant)) {
  fmlist[[pp]] <- nls(uptake ~ SSasymp(conc, Asym, lrc, c0),
      data = CO2, subset = Plant == pp)
}
## check the coefficients by plant
print(sapply(fmlist, coef), digits = 3)
}
\keyword{datasets}
