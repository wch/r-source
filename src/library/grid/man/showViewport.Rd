% File src/library/grid/man/showViewport.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2011 R Core Team
% Distributed under GPL 2 or later

\name{showViewport}
\alias{showViewport}
\title{
  Display grid viewports.
}
\description{
  Produces a graphical display of (by default) the current
  grid viewport tree.  It is also possible to display only
  specific viewports.  Each viewport is drawn as a rectangle
  and the leaf viewports are labelled with the viewport name.
}
\usage{
showViewport(vp = NULL, recurse = TRUE, depth = NULL,
             newpage = FALSE, leaves = FALSE,
             col = rgb(0, 0, 1, 0.2), fill = rgb(0, 0, 1, 0.1),
             label = TRUE, nrow = 3, ncol = nrow)
}
\arguments{
  \item{vp}{
    If \code{NULL}, the current viewport tree is displayed.
    Otherwise, a viewport (or vpList, or vpStack, or vpTree) or
    a vpPath that specifies which viewport to display.
  }
  \item{recurse}{
    Should the children of the specified viewport also be displayed?
  }
  \item{depth}{
    Only display viewports at the specified depth (may be a vector
    of depths).
  }
  \item{newpage}{
    Start a new page for the display?  Otherwise, the viewports
    are displayed on top of the current plot.
  }
  \item{leaves}{
    Produce a matrix of smaller displays, with each leaf viewport
    in its own display.
  }
  \item{col}{
    The colour used to draw the border of the rectangle for each
    viewport \emph{and} to draw the label for each viewport.
    If a vector, then the first colour is used for the
    top-level viewport, the second colour is used for its children,
    the third colour for their children, and so on.
  }
  \item{fill}{
    The colour used to fill each viewport.  May be a vector as per
    \code{col}.
  }
  \item{label}{
    Should the viewports be labelled (with the viewport name)?
  }
  \item{nrow, ncol}{
    The number of rows and columns when \code{leaves} is \code{TRUE}.
    Otherwise ignored.
  }
}

\seealso{
  \code{\link{viewport}} and
  \code{\link{grid.show.viewport}}
}
\examples{
showViewport(viewport(width=.5, height=.5, name="vp"))

grid.newpage()
pushViewport(viewport(width=.5, height=.5, name="vp"))
upViewport()
showViewport(vpPath("vp"))

showViewport(vpStack(viewport(width=.5, height=.5, name="vp1"),
                     viewport(width=.5, height=.5, name="vp2")),
             newpage=TRUE)

showViewport(vpStack(viewport(width=.5, height=.5, name="vp1"),
                     viewport(width=.5, height=.5, name="vp2")),
             fill=rgb(1:0, 0:1, 0, .1),
             newpage=TRUE)
}
\keyword{ dplot }
