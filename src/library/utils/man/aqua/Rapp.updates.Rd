% File src/library/utils/man/aqua/Rapp.updates.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Development Team
% Distributed under GPL 2 or later

\name{Rapp.updates}
\alias{Rapp.updates}
\title{Checks for newer versions of R with Aqua GUI on CRAN}
\description{
  This function is intended to check for new versions of R with Aqua GUI on CRAN.
}
\usage{
Rapp.updates()
}
\details{
  If a new version of R with Aqua GUI is found on CRAN, the user is asked about
  visiting CRAN for downloading the new version. If so, the corresponding
  CRAN url is opened in the favorite browser and no other action is taken.
  Is up to the user the final decision to download and install the new
  version of R with Aqua GUI.
}
\keyword{connection}
\keyword{utilities}
