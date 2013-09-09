% File src/library/grid/man/grid.grob.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{grid.grob}
\alias{grid.grob}
\alias{grob}
\alias{gTree}
\alias{grobTree}
\alias{childNames}
\alias{gList}
\alias{is.grob}
\title{Create a Grid Graphical Object}
\description{
  These functions create grid graphical objects.
}
\usage{
grid.grob(list.struct, cl = NULL, draw = TRUE)
grob(..., name = NULL, gp = NULL, vp = NULL, cl = NULL)
gTree(..., name = NULL, gp = NULL, vp = NULL, children = NULL,
      childrenvp = NULL, cl = NULL)
grobTree(..., name = NULL, gp = NULL, vp = NULL,
         childrenvp = NULL, cl = NULL)
childNames(gTree)
gList(...)
is.grob(x)
}
\arguments{
  \item{...}{For \code{grob} and \code{gTree}, the
    named slots describing important features of
    the graphical object.  For \code{gList} and \code{grobTree},
    a series of grob objects. }
  \item{list.struct}{a \code{\link{list}} (preferably with each element named).}
  \item{name}{a character identifier for the grob.  Used to find the
    grob on the display list and/or as a child of another grob. }
  \item{children}{a \code{"gList"} object. }
  \item{childrenvp}{a \code{\link{viewport}} object (or \code{NULL}).}
  \item{gp}{A gpar object, typically the output
    from a call to the function \code{gpar}.  This is basically
    a list of graphical parameter settings.}
  \item{vp}{a \code{\link{viewport}} object (or \code{NULL}).}
  \item{cl}{string giving the class attribute for the \code{list.struct}.}
  \item{draw}{logical indicating whether to produce graphical output.}
  \item{gTree}{a \code{"gTree"} object.}
  \item{x}{ An \R object. }
}
\details{
  These functions can be used to create a basic \code{"grob"},
  \code{"gTree"}, or \code{"gList"} object, or a new class derived from
  one of these.

  A grid graphical object (\dQuote{grob}) is a description of a graphical
  item.  These basic classes provide default behaviour for
  validating, drawing,
  and modifying graphical objects.  Both call the function
  \code{validDetails} to check that the object returned is coherent.

  A \code{"gTree"} can have other grobs as children;  when a gTree is drawn, it
  draws all of its children.  Before drawing its children, a gTree
  pushes its \code{childrenvp} slot and then navigates back up (calls
  \code{\link{upViewport}}) so that the children can specify their location
  within the \code{childrenvp} via a \code{\link{vpPath}}.

  Grob names need not be unique in general, but all children of a
  gTree must have different names.
  A grob name can be any string, though it is not
  advisable to use the \code{\link{gPath}} separator (currently
  \code{::}) in grob names.

  The function \code{childNames} returns the names of the grobs
  which are children of a gTree.

  All grid primitives (\code{\link{grid.lines}}, \code{grid.rect}, ...)
  and some higher-level grid components (e.g., \code{\link{grid.xaxis}}
  and \code{grid.yaxis}) are derived from these classes.

  \code{grobTree} is just a convenient wrapper for \code{gTree}
  when the only components of the gTree are grobs (so all
  unnamed arguments become children of the gTree).

  \code{grid.grob} is deprecated.
}
\value{
  An \R object of class \code{"grob"}, a \bold{gr}aphical \bold{ob}ject.
}
\author{Paul Murrell}
\seealso{
  \code{\link{grid.draw}},
  \code{\link{grid.edit}},
  \code{\link{grid.get}}.
}
\keyword{dplot}
