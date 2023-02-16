% File src/library/utils/man/utils-internal.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2023 R Core Team
% Distributed under GPL 2 or later

\name{utils-internal}
\alias{de.ncols}
\alias{de.restore}
\alias{de.setup}
\alias{limitedLabels}
\title{Internal utils Functions}
\description{
  Internal utils functions
}
\usage{
de.ncols(inlist) 
de.restore(inlist, ncols, coltypes, argnames, args) 
de.setup(ilist, list.names, incols) 
limitedLabels(value, maxwidth = getOption("width") - 5L) 
}
\details{
  These are not to be called by the user.
}
\keyword{ internal }
