% File src/library/datasets/man/quakes.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{quakes}
\docType{data}
\alias{quakes}
\title{Locations of Earthquakes off Fiji}
\description{
  The data set give the locations of 1000 seismic events of MB > 4.0.
  The events occurred in a cube near Fiji since 1964.
}
\usage{quakes}
\format{
  A data frame with 1000 observations on 5 variables.
  \tabular{rlll}{
    [,1] \tab lat      \tab numeric \tab Latitude of event\cr
    [,2] \tab long     \tab numeric \tab Longitude\cr
    [,3] \tab depth    \tab numeric \tab Depth (km)\cr
    [,4] \tab mag      \tab numeric \tab Richter Magnitude\cr
    [,5] \tab stations \tab numeric \tab Number of stations reporting
  }
}
\source{
  This is one of the Harvard PRIM-H project data sets.  They in turn
  obtained it from Dr. John Woodhouse, Dept. of Geophysics, Harvard
  University.
}
\details{
  There are two clear planes of seismic activity.  One is a major plate
  junction; the other is the Tonga trench off New Zealand.  These data
  constitute a subsample from a larger dataset of containing 5000
  observations.
}
\examples{
require(graphics)
pairs(quakes, main = "Fiji Earthquakes, N = 1000", cex.main = 1.2, pch = ".")
}
\keyword{datasets}
