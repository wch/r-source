% File src/library/utils/man/flush.console.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{flush.console}
\alias{flush.console}
\title{Flush Output to A Console}
\description{
  This does nothing except on console-based versions of \R.
  On the OS X and Windows GUIs, it ensures that the display of
  output in the console is current, even if output buffering is on.
}
\usage{
flush.console()
}

\keyword{ utilities }
