% File src/library/grid/man/grid.newpage.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{grid.newpage}
\alias{grid.newpage}
\title{Move to a New Page on a Grid Device}
\description{
  This function erases the current device or moves to a new page.
}
\usage{
grid.newpage(recording = TRUE)
}
\arguments{
  \item{recording}{A logical value to indicate whether the new-page
    operation should be saved onto the Grid display list.}
}
\details{
  The new page is painted with the fill colour
  (\code{\link{gpar}("fill")}), which is often transparent.  For devices
  with a \emph{canvas} colour (the on-screen devices \code{X11},
  \code{windows} and \code{quartz}), the page is first painted with the
  canvas colour and then the background colour.

  There are two hooks called \code{"before.grid.newpage"} and
  \code{"grid.newpage"} (see \code{\link{setHook}}).  The latter
  is used in the testing code to annotate the new page.  The hook
  function(s) are called with no argument.  (If the value is a character
  string, \code{get} is called on it from within the \pkg{grid}
  namespace.)
}
\value{
  None.
}
\author{Paul Murrell}
\seealso{
  \link{Grid}
}
\keyword{dplot}
