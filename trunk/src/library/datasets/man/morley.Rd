\name{morley}
\docType{data}
\alias{morley}
\title{Michaelson-Morley Speed of Light Data}
\description{
  The classical data of Michaelson and Morley on the speed of light.
  The data consists of five experiments, each consisting of 20
  consecutive \sQuote{runs}.  The response is the speed of light
  measurement, suitably coded.
}
\usage{morley}
\format{
  A data frame contains the following components:
  \describe{
    \item{\code{Expt}}{The experiment number, from 1 to 5.}
    \item{\code{Run}}{The run number within each experiment.}
    \item{\code{Speed}}{Speed-of-light measurement.}
  }
}
\details{
  The data is here viewed as a randomized block experiment with
  \sQuote{experiment} and \sQuote{run} as the factors.  \sQuote{run} may
  also be considered a quantitative variate to account for linear (or
  polynomial) changes in the measurement over the course of a single
  experiment.
}
\source{
  A. J. Weekes (1986)
  \emph{A Genstat Primer}.
  London: Edward Arnold.
}
\examples{
require(stats)
morley$Expt <- factor(morley$Expt)
morley$Run <- factor(morley$Run)
attach(morley)
plot(Expt, Speed, main = "Speed of Light Data", xlab = "Experiment No.")
fm <- aov(Speed ~ Run + Expt, data = morley)
summary(fm)
fm0 <- update(fm, . ~ . - Run)
anova(fm0, fm)
detach(morley)
}
\keyword{datasets}
