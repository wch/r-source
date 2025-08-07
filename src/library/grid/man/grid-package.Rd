% File src/library/grid/man/grid-package.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2025 R Core Team
% Distributed under GPL 2 or later

\name{grid-package}
\alias{grid-package}
\docType{package}
\title{
  The Grid Graphics Package
}
\description{
  A rewrite of the graphics layout capabilities, plus some
  support for interaction.
}
\details{
This package contains a graphics system which supplements
S-style graphics (see the \pkg{graphics} package).

Further information is available in the following
\link{vignettes}:
\tabular{ll}{
\code{grid} \tab \href{../doc/grid.pdf}{Introduction to \pkg{grid}} \cr
\code{displaylist} \tab \href{../doc/displaylist.pdf}{Display Lists in \pkg{grid}} \cr
\code{frame} \tab \href{../doc/frame.pdf}{Frames and packing grobs} \cr
\code{grobs} \tab \href{../doc/grobs.pdf}{Working with \pkg{grid} grobs} \cr
\code{interactive} \tab \href{../doc/interactive.pdf}{Editing \pkg{grid} Graphics} \cr
\code{locndimn} \tab \href{../doc/locndimn.pdf}{Locations versus Dimensions} \cr
\code{moveline} \tab \href{../doc/moveline.pdf}{Demonstrating move-to and line-to} \cr
\code{nonfinite} \tab \href{../doc/nonfinite.pdf}{How \pkg{grid} responds to non-finite values} \cr
\code{plotexample} \tab \href{../doc/plotexample.pdf}{Writing \pkg{grid} Code} \cr
\code{rotated} \tab \href{../doc/rotated.pdf}{Rotated Viewports} \cr
\code{saveload} \tab \href{../doc/saveload.pdf}{Persistent representations} \cr
\code{sharing} \tab \href{../doc/sharing.pdf}{Modifying multiple grobs simultaneously} \cr
\code{viewports} \tab \href{../doc/viewports.pdf}{Working with \pkg{grid} viewports} \cr
}

For a complete list of functions with individual help pages,
use \code{library(help="grid")}.
}
\author{

Paul Murrell \email{paul@stat.auckland.ac.nz}

Maintainer: R Core Team \email{R-core@r-project.org}

}
\references{
  \bibshow{R:Murrell:2005}
}
\keyword{package}
