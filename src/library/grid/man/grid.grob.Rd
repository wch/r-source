\name{grid.grob}
\alias{grid.grob}
\title{Create a Grid Graphical Object}
\description{
  Creates a Grid graphical object.
}
\usage{
grid.grob(list.struct, cl = NULL, draw = TRUE)
}
\arguments{
  \item{list.struct}{A list (preferably with each element named).}
  \item{cl}{A string giving the class attribute for the \code{list.struct}}
  \item{draw}{A logical value to indicate whether to produce
    graphical output.}
}
\details{
  A Grid graphical object provides a pointer to the
  \code{list.struct}.  This has the important consequence that
  copies of the graphical object refer to the same \code{list.struct}.
  
  All Grid primitives (\code{grid.lines}, \code{grid.rect}, ...) and
  some higher-level Grid functions (e.g., \code{grid.xaxis} and
  \code{grid.yaxis}) return graphical objects.

  Grid provides several useful functions for graphical objects
  (e.g., \code{grid.draw} and \code{grid.edit}) which are designed to make
  it easier to produce new graphical objects.
}
\value{
  An object of class \code{"grob"}.
}
\author{Paul Murrell}
\seealso{
  \code{\link{grid.draw}},
  \code{\link{grid.edit}},
  \code{\link{grid.get}},
  \code{\link{grid.copy}}.
}

\examples{
## Create a graphical object
l <- grid.lines(draw=FALSE)
## View the list.struct
grid.get(l)
## Copy by reference
l2 <- l
## Edit the common list.struct
grid.edit(l2, gp=gpar(col="green"))
## Copy by value
l3 <- grid.copy(l)
}
\keyword{dplot}
