\name{grid.edit}
\alias{grid.edit}
\title{Edit the Description of a Grid Graphical Object}
\description{
  Changes the value of one or more elements of the list structure
  within a graphical object and redraws the graphical object.
}
\usage{
grid.edit(grob, ..., redraw=TRUE)
}
\arguments{
  \item{grob}{An object of class \code{"grob"}.}
  \item{\dots}{Zero or more element-specifiers, plus a single
    new value or a list of new values.
    The new value is required.  Each specifier
    may be a single character or numeric value.  }
  \item{redraw}{A logical value to indicate whether to redraw the
    graphical object.}
}
\details{
  This function acts on the graphical object specified by \code{grob} and
  the element-specifiers.  It sets the values in the list structure
  of that graphical object
  which correspond to the new values.  If \code{redraw}
  is \code{TRUE} it then redraws everything to reflect the change.

  Before redrawing, it calls the generic function \code{edit.details},
  dispatching on the
  class of the list structure within the graphical object, so that
  further consequences of the editing (such as editing children of
  the graphical object) can occur.
}
\value{
  None.
}
\author{Paul Murrell}
\seealso{
  \code{\link{grid.grob}}
}
\examples{
grid.newpage()
xa <- grid.xaxis(vp=viewport(width=.5, height=.5))
grid.edit(xa, gp=gpar(col="red"))
grid.edit(xa, "ticks", gp=gpar(col="green"))
}
\keyword{dplot}
