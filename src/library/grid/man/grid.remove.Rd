\name{grid.remove}
\alias{grid.remove}
\alias{removeGrob}
\title{Remove a Grid Graphical Object}
\description{
  Remove a grob from a gTree or a descendant of a gTree.
}
\usage{
grid.remove(gPath, warn = TRUE, strict  =  FALSE, grep = FALSE,
            global = FALSE, allDevices = FALSE, redraw = TRUE)

removeGrob(gTree, gPath, strict = FALSE, grep = FALSE, global = FALSE,
           warn = TRUE) 
}
\arguments{
  \item{gTree}{A gTree object.}
  \item{gPath}{ A gPath object. For \code{grid.remove} this
    specifyies a gTree on the display list.  For \code{removeGrob} this
    specifies a descendant of the specified gTree. }
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
  \item{warn}{A logical to indicate whether failing to find the
    specified grob should trigger an error.  }
  \item{redraw}{A logical value to indicate whether to redraw the grob. }
}
\details{
  \code{removeGrob} copies the specified grob and returns a modified
  grob.
  
  \code{grid.remove} destructively modifies a grob on the display list.
  If \code{redraw}
  is \code{TRUE} it then redraws everything to reflect the change.

}
\value{
  \code{removeGrob} returns a grob object;  \code{grid.remove} returns \code{NULL}.
}
\author{Paul Murrell}
\seealso{
  \code{\link{grob}}, \code{\link{getGrob}},
  \code{\link{removeGrob}}, \code{\link{removeGrob}}.
}
\keyword{dplot}
