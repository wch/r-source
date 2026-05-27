% File src/library/datasets/man/stackloss.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2025 R Core Team
% Distributed under GPL 2 or later

\name{stackloss}
\docType{data}
\alias{stackloss}
\alias{stack.loss}
\alias{stack.x}
\title{\I{Brownlee}'s Stack Loss Plant Data}
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
    [,1] \tab \samp{Air Flow}   \tab Flow of cooling air\cr
    [,2] \tab \samp{Water Temp} \tab Cooling Water Inlet
    Temperature\cr
    [,3] \tab \samp{Acid Conc.} \tab Concentration of acid [per
    1000, minus 500]\cr
    [,4] \tab \code{stack.loss} \tab Stack loss\cr
  }

  For historical compatibility with S-PLUS, the data sets
  \code{stack.x}, a matrix with the first three (independent) variables
  of the data frame, and \code{stack.loss}, the numeric vector giving
  the fourth (dependent) variable, are also provided.
}
\source{
  \bibinfo{R:Brownlee:1965}{note}{Pages\sspace{}491--500}
  \bibshow{R:Brownlee:1965}
}
\details{
  \dQuote{Obtained from 21 days of operation of a plant for the
    oxidation of ammonia (\I{NH\eqn{_3}{3}}) to nitric acid
    (\I{HNO\eqn{_3}{3}}).  The nitric oxides produced are absorbed in a
    countercurrent absorption tower}.
  (\I{Brownlee}, cited by \I{Dodge}, slightly reformatted by MM.)

  \samp{Air Flow} represents the rate of operation of the plant.
  \samp{Water Temp} is the temperature of cooling water circulated
  through coils in the absorption tower.
  \samp{Acid Conc.} is the concentration of the acid circulating, minus
  50, times 10: that is, 89 corresponds to 58.9 per cent acid.
  \code{stack.loss} (the dependent variable) is 10 times the percentage
  of the ingoing ammonia to the plant that escapes from the absorption
  column unabsorbed; that is, an (inverse) measure of the over-all
  efficiency of the plant.
}
\references{
  \bibshow{R:Becker+Chambers+Wilks:1988, R:Dodge:1996}
}
\examples{
require(stats)
summary(lm.stack <- lm(stack.loss ~ stack.x))
}
\keyword{datasets}

