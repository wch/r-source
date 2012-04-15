% File src/library/datasets/man/stackloss.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{stackloss}
\docType{data}
\alias{stackloss}
\alias{stack.loss}
\alias{stack.x}
\title{Brownlee's Stack Loss Plant Data}
\description{
  Operational data of a plant for the oxidation of ammonia to nitric
  acid.
}
\usage{
stackloss

stack.x
stack.loss
}
\format{
  \code{stackloss} is a data frame with 21 observations on 4 variables.

  \tabular{rll}{
    [,1] \tab \code{Air Flow}   \tab Flow of cooling air\cr
    [,2] \tab \code{Water Temp} \tab Cooling Water Inlet
    Temperature\cr
    [,3] \tab  \code{Acid Conc.} \tab Concentration of acid [per
    1000, minus 500]\cr
    [,4] \tab  \code{stack.loss} \tab Stack loss\cr
  }

  For compatibility with S-PLUS, the data sets \code{stack.x}, a matrix
  with the first three (independent) variables of the data frame, and
  \code{stack.loss}, the numeric vector giving the fourth (dependent)
  variable, are provided as well.
}
\source{
  Brownlee, K. A. (1960, 2nd ed. 1965)
  \emph{Statistical Theory and Methodology in Science and Engineering}.
  New York: Wiley. pp. 491--500.
}
\details{
  \dQuote{Obtained from 21 days of operation of a plant for the
    oxidation of ammonia (NH\eqn{_3}{3}) to nitric acid
    (HNO\eqn{_3}{3}).  The nitric oxides produced are absorbed in a
    countercurrent absorption tower}.
  (Brownlee, cited by Dodge, slightly reformatted by MM.)

  \code{Air Flow} represents the rate of operation of the plant.
  \code{Water Temp} is the temperature of cooling water circulated
  through coils in the absorption tower.
  \code{Acid Conc.} is the concentration of the acid circulating, minus
  50, times 10: that is, 89 corresponds to 58.9 per cent acid.
  \code{stack.loss} (the dependent variable) is 10 times the percentage
  of the ingoing ammonia to the plant that escapes from the absorption
  column unabsorbed; that is, an (inverse) measure of the over-all
  efficiency of the plant.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.

  Dodge, Y. (1996)
  The guinea pig of multiple regression. In:
  \emph{Robust Statistics, Data Analysis, and Computer Intensive
    Methods; In Honor of Peter Huber's 60th Birthday}, 1996,
  \emph{Lecture Notes in Statistics} \bold{109}, Springer-Verlag, New York.
}
\examples{
require(stats)
summary(lm.stack <- lm(stack.loss ~ stack.x))
}
\keyword{datasets}

