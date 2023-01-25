% File src/library/grid/man/grid.glyph.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2020 R Core Team
% Distributed under GPL 2 or later

\name{grid.glyph}
\alias{grid.glyph}
\alias{glyphGrob}
\title{Draw Typeset Glyphs}
\description{
  These functions create and draw a set of typeset glyphs.
}
\usage{
grid.glyph(...)
glyphGrob(glyphInfo,
          x=.5, y=.5, default.units="npc",
          hjust="centre", vjust="centre",
          gp=gpar(), vp=NULL, name=NULL)
}
\arguments{
  \item{glyphInfo}{An \code{"RGlyphInfo"} object from a call to
    \code{\link{glyphInfo}}.}
  \item{x}{A numeric vector or unit object specifying x-location.}
  \item{y}{A numeric vector or unit object specifying y-location.}
  \item{default.units}{A string indicating the default units to use
    if \code{x}, \code{y}, \code{width}, or \code{height}
    are only given as numeric vectors.}
  \item{hjust, vjust}{The justification of the glyphs
    relative to the (\code{x}, \code{y}) location.
    Can be \code{character} (e.g., \code{"left"}),
    numeric (e.g., \code{0}), or the result of a call to
    \code{\link{glyphJust}}.
  }
  \item{name}{ A character identifier. }
  \item{gp}{An object of class \code{"gpar"}, typically the output
    from a call to the function \code{\link{gpar}}.  This is basically
    a list of graphical parameter settings.}
  \item{vp}{A Grid viewport object (or NULL).}
  \item{\dots}{Arguments passed to \code{glyphGrob()}.}
}
\details{
  Both functions create a glyph grob (a graphical object describing
  glyphs), but only \code{grid.glyph}
  draws the glyphs.

}
\value{
  A glyph grob. 
}
\author{Paul Murrell}
\seealso{
  \link{Grid},
  \code{\link{glyphInfo}}
}
\keyword{dplot}
