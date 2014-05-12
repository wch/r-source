% File src/library/datasets/man/morley.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\newcommand{\CRANpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}

\name{morley}
\docType{data}
\alias{morley}
\title{Michelson Speed of Light Data}
\description{
  A classical data of Michelson (but not this one with Morley) on
  measurements done in 1879 on the speed of light.  The data consists of
  five experiments, each consisting of 20 consecutive \sQuote{runs}.
  The response is the speed of light measurement, suitably coded
  (km/sec, with \code{299000} subtracted).
}
\usage{
morley
}
\format{
  A data frame with 100 observations on the following 3 variables.
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
\note{
  This is the same dataset as \code{michelson} in package
  \CRANpkg{MASS}.
}
\source{
  A. J. Weekes (1986)
  \emph{A Genstat Primer}.
  London: Edward Arnold.

  S. M. Stigler (1977)
  Do robust estimators work with real data?
  \emph{Annals of Statistics} \bold{5}, 1055--1098. (See Table 6.)

  A. A. Michelson (1882)
  Experimental determination of the velocity of light made at the United
  States Naval Academy, Annapolis.
  \emph{Astronomic Papers} \bold{1} 135--8.
  U.S. Nautical Almanac Office.  (See Table 24.)
}
\examples{
require(stats); require(graphics)
michelson <- transform(morley,
                       Expt = factor(Expt), Run = factor(Run))
xtabs(~ Expt + Run, data = michelson)  # 5 x 20 balanced (two-way)
plot(Speed ~ Expt, data = michelson,
     main = "Speed of Light Data", xlab = "Experiment No.")
fm <- aov(Speed ~ Run + Expt, data = michelson)
summary(fm)
fm0 <- update(fm, . ~ . - Run)
anova(fm0, fm)
}
\keyword{datasets}
