\name{stackloss}
\alias{stackloss}
\alias{stack.loss}
\alias{stack.x}
\title{Brownlee's Stack Loss Plant Data}
\usage{data(stackloss)}
\description{Operational data of a plant for the oxidation of ammonia to
  nitric acid.}
\format{A data frame with 21 observations on 4 variables.
  \tabular{rlll}{
    [,1] \tab x1 \tab \code{Air.Flow}   \tab Flow of cooling air\cr
    [,2] \tab x2 \tab \code{Water.Temp} \tab Cooling Water Inlet
    Temperature\cr
    [,3] \tab x3 \tab \code{Acid.Conc.} \tab Concentration of acid [per
    1000, minus 500]\cr
    [,4] \tab y  \tab \code{stack.loss} \tab Stack loss\cr
  }
}
\source{
  Brownlee, K. A. (1960, 2nd ed. 1965)
  \emph{Statistical Theory and Methodology in Science and Engineering.}
  New York: Wiley. pp. 491--500.
}
\details{
  ``Obtained from 21 days of operation of a plant for the oxidation
  of ammonia (NH\eqn{_3}{3}) to nitric acid (HNO\eqn{_3}{3}).
  The nitric oxides produced are absorbed in a countercurrent absorption
  tower.''  (Brownlee, cited by Dodge, slightly reformatted by MM.)

  \code{x1} represents the rate of operation of the plant.
  \code{x2} is the temperature of cooling water circulated through coils
  in the absorption tower.
  \code{x3} is the concentration of the acid circulating, minus 50,
  times 10: that is, 89 corresponds to 58.9 per cent acid.
  \code{y} (the dependent variable) is 10 times the percentage of the
  ingoing ammonia to the plant that escapes from the absorption column
  unabsorbed; that is, an (inverse) measure of the over-all efficiency
  of the plant.
}
\references{
  Dodge, Y. (1996)
  The guinea pig of multiple regression. In:
  \emph{Robust Statistics, Data Analysis, and Computer Intensive
    Methods; In Honor of Peter Huber's 60th Birthday}, 1996,
  \emph{Lecture Notes in Statistics} \bold{109}, Springer-Verlag, New York.
}
\examples{
data(stackloss)
summary(lm.stack <- lm(stack.loss ~ stack.x))
}
\keyword{datasets}

