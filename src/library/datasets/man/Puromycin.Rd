\name{Puromycin}
\docType{data}
\alias{Puromycin}
\title{Reaction velocity of an enzymatic reaction}
\description{
  The \code{Puromycin} data frame has 23 rows and 3 columns of the
  reaction velocity versus substrate concentration in an enzymatic
  reaction involving untreated cells or cells treated with Puromycin.
}
\usage{Puromycin}
\format{
  This data frame contains the following columns:
  \describe{
    \item{conc}{
      a numeric vector of substrate concentrations (ppm)
    }
    \item{rate}{
      a numeric vector of instantaneous reaction rates (counts/min/min)
    }
    \item{state}{
      a factor with levels
      \code{treated} 
      \code{untreated} 
    }
  }
}
\details{
  Data on the \dQuote{velocity} of an enzymatic reaction were obtained
  by Treloar (1974).  The number of counts per minute of radioactive
  product from the reaction was measured as a function of substrate
  concentration in parts per million (ppm) and from these counts the
  initial rate, or \dQuote{velocity,} of the reaction was calculated
  (counts/min/min).  The experiment was conducted once with the enzyme
  treated with Puromycin, and once with the enzyme untreated.
}
\source{
  Bates, D.M. and Watts, D.G. (1988),
  \emph{Nonlinear Regression Analysis and Its Applications},
  Wiley, Appendix A1.3.

  Treloar, M. A. (1974), \emph{Effects of Puromycin on
    Galactosyltransferase in Golgi Membranes}, M.Sc. Thesis, U. of
  Toronto. 
}
\examples{
plot(rate ~ conc, data = Puromycin, las = 1,
     xlab = "Substrate concentration (ppm)",
     ylab = "Reaction velocity (counts/min/min)",
     pch = as.integer(Puromycin$state),
     col = as.integer(Puromycin$state),
     main = "Puromycin data and fitted Michaelis-Menten curves")
## simplest form of fitting the Michaelis-Menten model to these data
fm1 <- nls(rate ~ Vm * conc/(K + conc), data = Puromycin,
           subset = state == "treated",
           start = c(Vm = 200, K = 0.05), trace = TRUE)
fm2 <- nls(rate ~ Vm * conc/(K + conc), data = Puromycin,
           subset = state == "untreated",
           start = c(Vm = 160, K = 0.05), trace = TRUE)
summary(fm1)
summary(fm2)
## using partial linearity
fm3 <- nls(rate ~ conc/(K + conc), data = Puromycin,
           subset = state == "treated", start = c(K = 0.05),
           algorithm = "plinear", trace = TRUE)
## using a self-starting model
fm4 <- nls(rate ~ SSmicmen(conc, Vm, K), data = Puromycin,
           subset = state == "treated")
summary(fm4)
## add fitted lines to the plot
conc <- seq(0, 1.2, len = 101)
lines(conc, predict(fm1, list(conc = conc)), lty = 1, col = 1)
lines(conc, predict(fm2, list(conc = conc)), lty = 2, col = 2)
legend(0.8, 120, levels(Puromycin$state),
       col = 1:2, lty = 1:2, pch = 1:2)
}
\keyword{datasets}
