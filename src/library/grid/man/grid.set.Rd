\name{grid.set}
\alias{grid.set}
\alias{setGrob}
\title{Set a Grid Graphical Object}
\description{
  Replace a grob or a descendant of a grob.

}
\usage{
grid.set(gPath, newGrob, strict = FALSE, grep = FALSE,
         redraw = TRUE)
 
setGrob(gTree, gPath, newGrob, strict = FALSE, grep = FALSE) 
}
\arguments{
  \item{gTree}{A gTree object.}
  \item{gPath}{ A gPath object. For \code{grid.set} this
    specifyies a grob on the display list.  For \code{setGrob} this
    specifies a descendant of the specified gTree. }
  \item{newGrob}{ A grob object. }
  \item{strict}{ A boolean indicating whether the gPath must be
    matched exactly. }
  \item{grep}{A boolean indicating whether the \code{gPath} should
    be treated as a regular expression.  Values are recycled across
    elements of the \code{gPath} (e.g., \code{c(TRUE, FALSE)} means
    that every odd element of the \code{gPath} will be treated as
    a regular expression).
  }
  \item{redraw}{A logical value to indicate whether to redraw the grob. }
}
\details{
  \code{setGrob} copies the specified grob and returns a modified
  grob.
  
  \code{grid.set} destructively replaces a grob on the display list.
  If \code{redraw}
  is \code{TRUE} it then redraws everything to reflect the change.

  These functions should not normally be called by the user.
}
\value{
  \code{setGrob} returns a grob object;  \code{grid.set} returns \code{NULL}.
}
\author{Paul Murrell}
\seealso{
  \code{\link{grid.grob}}.
}
\keyword{dplot}
