\name{attenu}
\title{The Joyner-Boore Attenuation Data}
\usage{data(attenu)}
\alias{attenu}
\format{A dataframe with 182 observations on 5 variables.
  \tabular{rlll}{
    [,1] \tab event    \tab numeric  \tab Event Number \cr
    [,2] \tab mag      \tab numeric  \tab Moment Magnitude \cr
    [,3] \tab station  \tab factor   \tab Station Number \cr
    [,4] \tab dist     \tab numeric  \tab Station-hypocenter distance (km)\cr
    [,5] \tab accel    \tab numeric  \tab Peak acceleration (g)\cr
  }
}
\source{
  Joyner, W.B., D.M. Boore and R.D. Porcella (1981).  Peak horizontal
  acceleration and velocity from strong-motion records including
  records from the 1979 Imperial Valley, California earthquake.  USGS
  Open File report 81-365. Menlo Park, Ca.
}
\description{
  This data gives peak accelerations measured at various observation
  stations for 23 earthquakes in California. The data have been used
  by various workers to estimate the attenuating affect of distance
  on ground acceleration.
}
\references{
  Boore, D.M. and W.B. Joyner (1982).  The empirical prediction of
  ground motion, \emph{Bull. Seism. Soc. Am.} \bold{72}, S269-S268.

  Bolt B.A. and N.A. Abrahamson (1982).  New attenuation relations
  for peak and expected accelerations of strong ground motion, \emph{Bull.
    Seism. Soc. Am.} \bold{72}, 2307-2321.

  Bolt B.A. and N.A. Abrahamson (1983).  Reply to W.B. Joyner and
  D.M. Boore's "Comments on `new attenuation relations for peak and
  expected accelerations for peak and expected accelerations of strong
  ground motion'", \emph{Bull. Seism. Soc. Am.} \bold{73} , 1481-1483.

  Brillinger, D.R. and H.K. Preisler (1984).  An exploratory analysis
  of the Joyner-Boore attenuation data, \emph{Bull. Seism. Soc. Am.} \bold{74},
  1441-1449.

  Brillinger, D.R. and H.K. Preisler (1984).  Further analysis of
  the Joyner-Boore attenuation data, Manuscript.
}
\examples{
data(attenu)
## check the data class of the variables
sapply(attenu, data.class)
summary(attenu)
pairs(attenu, main = "attenu data")
coplot(accel ~ dist | as.factor(event), data = attenu, show = FALSE)
coplot(log(accel) ~ log(dist) | as.factor(event),
       data = attenu, panel = panel.smooth, show.given = FALSE)
}
\keyword{datasets}
