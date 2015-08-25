% File src/library/grid/man/grid.ls.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{grid.ls}
\alias{grid.ls}
\alias{nestedListing}
\alias{pathListing}
\alias{grobPathListing}
\title{ List the names of grobs or viewports }
\description{
  Return a listing of the names of grobs or viewports.

  This is a generic function with methods for grobs (including
  gTrees) and viewports (including vpTrees).
}
\usage{
grid.ls(x=NULL, grobs=TRUE, viewports=FALSE, fullNames=FALSE,
        recursive=TRUE, print=TRUE, flatten=TRUE, ...)

nestedListing(x, gindent="  ", vpindent=gindent)
pathListing(x, gvpSep=" | ", gAlign=TRUE)
grobPathListing(x, ...)
}
\arguments{
  \item{x}{A grob or viewport or \code{NULL}.  If \code{NULL}, the current
    grid display list is listed.

    For print functions, this should be the result of a call to
    \code{grid.ls}.}
  \item{grobs}{A logical value indicating whether to list grobs.}
  \item{viewports}{A logical value indicating whether to list
    viewports.}
  \item{fullNames}{A logical value indicating whether to embellish
    object names with information about the object type.}
  \item{recursive}{A logical value indicating whether recursive
    structures should also list their children.}
  \item{print}{A logical indicating whether to print the listing
    or a function that will print the listing.}
  \item{flatten}{A logical value indicating whether to flatten
    the listing.  Otherwise a more complex hierarchical object is
    produced.}
  \item{gindent}{The indent used to show nesting in the output for
    grobs.}
  \item{vpindent}{The indent used to show nesting in the output for
    viewports.}
  \item{gvpSep}{The string used to separate viewport paths from grob
    paths.}
  \item{gAlign}{Logical indicating whether to align the left hand
    edge of all grob paths.}
  \item{...}{Arguments passed to the \code{print} function.}
}
\details{
  If the argument \code{x} is \code{NULL}, the current contents
  of the grid display list are listed (both viewports and grobs).
  In other words, all objects representing the current scene
  are listed.

  Otherwise, \code{x} should be a grob or a viewport.

  The default behaviour of this function is to print information
  about the grobs in the current scene.  It is also possible to
  add information about the viewports in the scene.  By default,
  the listing is recursive, so all children of gTrees and all
  nested viewports are reported.

  The format of the information can be controlled via the \code{print}
  argument, which can be given a function to perform the formatting.
  The \code{nestedListing} function produces a line per grob or
  viewport, with indenting used to show nesting.  The \code{pathListing}
  function produces a line per grob or viewport, with viewport paths
  and grob paths used to show nesting.  The \code{grobPathListing}
  is a simple derivation that only shows lines for grobs.  The user
  can define new functions.
}
\value{
  The result of this function is either a \code{"gridFlatListing"}
  object (if \code{flatten} is \code{TRUE}) or a \code{"gridListing"}
  object.

  The former is a simple (flat) list of vectors.  This is convenient,
  for example,
  for working programmatically with the list of grob and viewport
  names, or for writing a new display function for the listing.

  The latter is a more complex hierarchical object (list of lists),
  but it does contain more detailed information so may be of use for
  more advanced customisations.
}
\seealso{
  \code{\link{grob}}
  \code{\link{viewport}}
}
\author{ Paul Murrell }
\examples{
# A gTree, called "parent", with childrenvp vpTree (vp2 within vp1)
# and child grob, called "child", with vp vpPath (down to vp2)
sampleGTree <- gTree(name="parent",
                     children=gList(grob(name="child", vp="vp1::vp2")),
                     childrenvp=vpTree(parent=viewport(name="vp1"),
                                       children=vpList(viewport(name="vp2"))))
grid.ls(sampleGTree)
# Show viewports too
grid.ls(sampleGTree, view=TRUE)
# Only show viewports
grid.ls(sampleGTree, view=TRUE, grob=FALSE)
# Alternate displays
# nested listing, custom indent
grid.ls(sampleGTree, view=TRUE, print=nestedListing, gindent="--")
# path listing
grid.ls(sampleGTree, view=TRUE, print=pathListing)
# path listing, without grobs aligned
grid.ls(sampleGTree, view=TRUE, print=pathListing, gAlign=FALSE)
# grob path listing
grid.ls(sampleGTree, view=TRUE, print=grobPathListing)
# path listing, grobs only
grid.ls(sampleGTree, print=pathListing)
# path listing, viewports only
grid.ls(sampleGTree, view=TRUE, grob=FALSE, print=pathListing)
# raw flat listing
str(grid.ls(sampleGTree, view=TRUE, print=FALSE))
}
\keyword{ dplot }
