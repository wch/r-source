% File src/library/grid/man/grid.remove.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2017 R Core Team
% Distributed under GPL 2 or later

\name{grid.remove}
\alias{grid.remove}
\alias{grid.gremove}
\alias{removeGrob}
\title{Remove a Grid Graphical Object}
\description{
  Remove a grob from a gTree or a descendant of a gTree.
}
\usage{
grid.remove(gPath, warn = TRUE, strict = FALSE, grep = FALSE,
            global = FALSE, allDevices = FALSE, redraw = TRUE)

grid.gremove(\dots, grep = TRUE, global = TRUE)

removeGrob(gTree, gPath, strict = FALSE, grep = FALSE,
           global = FALSE, warn = TRUE)
}
\arguments{
  \item{gTree}{A gTree object.}
  \item{gPath}{a gPath object. For \code{grid.remove} this
    specifies a gTree on the display list.  For \code{removeGrob} this
    specifies a descendant of the specified \code{gTree}.}
  \item{strict}{a \code{\link{logical}} indicating whether the \code{gPath} must be
    matched exactly.}
  \item{grep}{a \code{\link{logical}} indicating whether the \code{gPath} should
    be treated as a regular expression.  Values are recycled across
    elements of the \code{gPath} (e.g., \code{c(TRUE, FALSE)} means
    that every odd element of the \code{gPath} will be treated as
    a regular expression).
  }
  \item{global}{a \code{\link{logical}} indicating whether the function should affect
    just the first match of the \code{gPath}, or whether all matches
    should be affected.
  }
  \item{allDevices}{a \code{\link{logical}} indicating whether all open
    devices should be searched for matches, or just the current device.
    NOT YET IMPLEMENTED.
  }
  \item{warn}{A logical to indicate whether failing to find the
    specified grob should trigger an error.  }
  \item{redraw}{A logical value to indicate whether to redraw the grob. }
  \item{\dots}{arguments passed to \code{\link{grid.get}}.}
}
\details{
  \code{removeGrob} copies the specified grob and returns a modified
  grob.

  \code{grid.remove} destructively modifies a grob on the display list.
  If \code{redraw}
  is \code{TRUE} it then redraws everything to reflect the change.

  \code{grid.gremove} (\code{g} for global) is just a convenience wrapper for
  \code{grid.remove} with different defaults.
}
\value{
  \code{removeGrob} returns a grob object;  \code{grid.remove} returns
  \code{NULL}.
}
\author{Paul Murrell}
\seealso{
  \code{\link{grob}}, \code{\link{getGrob}}.
}
\keyword{dplot}
