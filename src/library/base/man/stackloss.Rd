\name{stackloss}
\title{Brownlee's Stack Loss Plant Data}
\usage{data(stackloss)}
\alias{stackloss}
\format{A data frame with  21 observations on 4 variables.
  \tabular{rlll}{
    [,1] \tab x1 \tab Air.Flow  \tab Flow of cooling air\cr
    [,2] \tab x2 \tab Water.Temp \tab Cooling Water Inlet\cr
  \tab  \tab   \tab Temperature\cr
    [,3] \tab x3 \tab Acid.Conc. \tab Concentration of acid\cr
  \tab  \tab   \tab [per 1000, minus 500]\cr
    [,4] \tab y \tab stack.loss  \tab Stack loss\cr
  }
}
\source{Brownlee, K.A. (1960, 2nd ed. 1965)
 \emph{Statistical Theory and Methodology in Science and Engineering};
 John Wiley, NY;  pp. 491--500
}
\description{(Brownlee, cited by Dodge, slightly reformatted by MM):
``Obtained from 21 days of operation of a plant for the oxidation
 of ammonia (NH3) to nitric acid (HNO3).
 The nitric oxides produced are absorbed in a countercurrent absorption
 tower.

 \code{x1} represents the rate of operation of the plant.
 \code{x2} is the temperature of cooling water circulated through coils in the
      absorption tower.
 \code{x3} is the concentration of the acid circulating,
      minus 50, times 10: that is, 89 corresponds to 58.9 per cent acid.
 \code{y} (the dependent variable) is 10 times the percentage of the ingoing
      ammonia to the plant that escapes from the absorption column
      unabsorbed; that is, an (inverse) measure of the over-all efficiency
      of the plant.''

}
\references{Yadolah Dodge, The Guinea Pig of Multiple Regression,
  Robust Statistics, Data Analysis, and Computer
  Intensive Methods; In Honor of Peter Huber's 60th Birthday, 1996,
  Lecture Notes in Statistics 109, Springer-Verlag, New York.
}
\examples{
data(stackloss)
summary(lm.stack <- lm(stack.loss ~ stack.x))
}
\keyword{datasets}

