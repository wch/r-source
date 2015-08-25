% File src/library/grDevices/man/cm.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{cm}
\alias{cm}
\title{Unit Transformation}
\description{
  Translates from inches to cm (centimeters).
}
\usage{
cm(x)
}
\arguments{
  \item{x}{numeric vector}
}
\examples{
cm(1)  # = 2.54

## Translate *from* cm *to* inches:

10 / cm(1) # -> 10cm  are 3.937 inches
}
\keyword{dplot}
