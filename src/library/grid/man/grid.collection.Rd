\name{grid.collection}
\alias{grid.collection}
\title{Create a Coherent Group of Grid Graphical Objects}
\description{
  This function creates a graphical object which contains
  several other graphical objects.  When it is drawn, it draws
  all of its children.  

  It may be convenient to name the elements of the collection.
}
\usage{
grid.collection(..., gp=gpar(), draw=TRUE, vp=NULL)
}
\arguments{
  \item{\dots}{Zero or more objects of class \code{"grob"}.}
  \item{gp}{An object of class \code{gpar}, typically the output
    from a call to the function \code{gpar}.  This is basically
    a list of graphical parameter settings.}
  \item{draw}{A logical value to indicate whether to produce
    graphical output.}
  \item{vp}{A Grid viewport object (or NULL).}
}
\value{
  An object of class \code{"grob"} containing a list structure
  of class \code{"collection"}.
}
\author{Paul Murrell}
\seealso{
  \code{\link{grid.grob}}.
}
\examples{
grid.newpage()
lc <- grid.collection(l1=grid.lines(draw=FALSE),
                      l2=grid.lines(c(.5, .5), draw=FALSE))
grid.edit(lc, gp=gpar(col="blue"))
grid.edit(lc, "children", "l2", gp=gpar(col="red"))
}
\keyword{dplot}
