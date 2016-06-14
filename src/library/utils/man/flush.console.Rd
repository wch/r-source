% File src/library/utils/man/flush.console.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{flush.console}
\alias{flush.console}
\title{Flush Output to A Console}
\description{
  This does nothing except on console-based versions of \R.
  On the macOS and Windows GUIs, it ensures that the display of
  output in the console is current, even if output buffering is on.
}
\usage{
flush.console()
}

\keyword{ utilities }
