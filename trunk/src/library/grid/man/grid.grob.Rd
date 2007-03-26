\name{grid.grob}
\alias{grid.grob}
\alias{grob}
\alias{gTree}
\alias{childNames}
\alias{gList}
\title{Create a Grid Graphical Object}
\description{
  These functions create grid graphical objects.
}
\usage{
grid.grob(list.struct, cl = NULL, draw = TRUE)
grob(..., name = NULL, gp = NULL, vp = NULL, cl = NULL) 
gTree(..., name = NULL, gp = NULL, vp = NULL, children = NULL, 
      childrenvp = NULL, cl = NULL)
childNames(gTree)
gList(...)
}
\arguments{
  \item{...}{ For \code{grob} and \code{gTree}, the
    named slots describing important features of
    the graphical object. For \code{gList}, a series of grob objects. }
  \item{list.struct}{A list (preferably with each element named).}
  \item{name}{ A character identifier for the grob.  Used to find the
    grob on the display list and/or as a child of another grob. }
  \item{children}{ A gList object. }
  \item{childrenvp}{ A viewport object (or \code{NULL}).}
  \item{gp}{A gpar object, typically the output
    from a call to the function \code{gpar}.  This is basically
    a list of graphical parameter settings.}
  \item{vp}{A viewport object (or \code{NULL}).}
  \item{cl}{A string giving the class attribute for the \code{list.struct}}
  \item{draw}{A logical value to indicate whether to produce
    graphical output.}
  \item{gTree}{A gTree object.}
}
\details{
  These functions can be used to create a basic grob, gTree, or gList
  object, or a new class derived from one of these.
  
  A grid graphical object (grob) is a description of a graphical
  item.  These basic classes provide default behaviour for
  validating, drawing,
  and modifying graphical objects.  Both call the function
  \code{validDetails} to check that the object returned is coherent.

    A gTree can have other grobs as children;  when a gTree is drawn, it
  draws all of its children.  Before drawing its children, a gTree
  pushes its childrenvp slot and then navigates back up (calls
  \code{upViewport}) so that the children can specify their location
  within the childrenvp via a vpPath.

  Grob names need not be unique in general, but all children of a
  gTree must have different names.  
    A grob name can be any string, though
    it is not
    advisable to use the gPath separator  (currently \code{::}) in
    grob names.

  The function \code{childNames} returns the names of the grobs
  which are children of a gTree.
  
  All grid primitives (\code{grid.lines}, \code{grid.rect}, ...) and
  some higher-level grid components (e.g., \code{grid.xaxis} and
  \code{grid.yaxis}) are derived from these classes.

  \code{grid.grob} is deprecated.

}
\value{
  A grob object.
}
\author{Paul Murrell}
\seealso{
  \code{\link{grid.draw}},
  \code{\link{grid.edit}},
  \code{\link{grid.get}}.
}

\examples{
}
\keyword{dplot}
