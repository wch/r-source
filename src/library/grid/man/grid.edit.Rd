\name{grid.edit}
\alias{grid.edit}
\alias{editGrob}
\title{Edit the Description of a Grid Graphical Object}
\description{
  Changes the value of one of the slots of a grob
 and redraws the grob.
}
\usage{
grid.edit(gPath, ..., strict = FALSE, grep = FALSE, global = FALSE,
          allDevices = FALSE, redraw = TRUE) 
editGrob(grob, gPath = NULL, ..., strict = FALSE, grep = FALSE,
         global = FALSE) 
}
\arguments{
  \item{grob}{A grob object.}
  \item{\dots}{Zero or more named arguments specifying new slot values.}
  \item{gPath}{ A gPath object. For \code{grid.edit} this
    specifies a grob on the display list.  For \code{editGrob} this
    specifies a descendant of the specified grob. }
  \item{strict}{ A boolean indicating whether the gPath must be
    matched exactly. }
  \item{grep}{A boolean indicating whether the \code{gPath} should
    be treated as a regular expression.  Values are recycled across
    elements of the \code{gPath} (e.g., \code{c(TRUE, FALSE)} means
    that every odd element of the \code{gPath} will be treated as
    a regular expression).
  }
  \item{global}{ A boolean indicating whether the function should affect
    just the first match of the \code{gPath}, or whether all matches
    should be affected.
  }
  \item{allDevices}{ A boolean indicating whether all open devices
    should
    be searched for matches, or just the current device.
    NOT YET IMPLEMENTED.
  }
  \item{redraw}{A logical value to indicate whether to redraw the grob. }
}
\details{
  \code{editGrob} copies the specified grob and returns a modified
  grob.
  
  \code{grid.edit} destructively modifies a grob on the display list.
  If \code{redraw}
  is \code{TRUE} it then redraws everything to reflect the change.

  Both functions call \code{editDetails} to allow a grob to perform
  custom actions and \code{validDetails} to check that the modified grob
  is still coherent.
  
}
\value{
  \code{editGrob} returns a grob object;  \code{grid.edit} returns \code{NULL}.
}
\author{Paul Murrell}
\seealso{
  \code{\link{grob}}, \code{\link{getGrob}},
  \code{\link{addGrob}}, \code{\link{removeGrob}}.
}
\examples{
grid.newpage()
grid.xaxis(name = "xa", vp = viewport(width=.5, height=.5))
grid.edit("xa", gp = gpar(col="red"))
# won't work because no ticks (at is NULL)
try(grid.edit(gPath("xa", "ticks"), gp = gpar(col="green")))
grid.edit("xa", at = 1:4/5)
# Now it should work
try(grid.edit(gPath("xa", "ticks"), gp = gpar(col="green")))
}
\keyword{dplot}
