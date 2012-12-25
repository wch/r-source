% File src/library/grid/man/showGrob.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2011 R Core Team
% Distributed under GPL 2 or later

\name{showGrob}
\alias{showGrob}
\title{
  Label grid grobs.
}
\description{
  Produces a graphical display of (by default) the current grid scene,
  with labels showing the names of each grob in the scene.
  It is also possible to label only specific grobs in the scene.
}
\usage{
showGrob(x = NULL,
         gPath = NULL, strict = FALSE, grep = FALSE,
         recurse = TRUE, depth = NULL,
         labelfun = grobLabel, ...)
}
\arguments{
  \item{x}{
    If \code{NULL}, the current grid scene is labelled.
    Otherwise, a grob (or gTree) to draw and then label.
  }
  \item{gPath}{
    A path identifying a subset of the current scene or grob
    to be labelled.
  }
  \item{strict}{
    Logical indicating whether the gPath is strict.
  }
  \item{grep}{
    Logical indicating whether the gPath is a regular expression.
  }
  \item{recurse}{
    Should the children of gTrees also be labelled?
  }
  \item{depth}{
    Only display grobs at the specified depth (may be a vector
    of depths).
  }
  \item{labelfun}{
    Function used to generate a label from each grob.
  }
  \item{...}{
    Arguments passed to \code{labelfun} to control fine details
    of the generated label.
  }
}
\details{
  None of the labelling is recorded on the grid display list so the
  original scene can be reproduced by calling \code{grid.refresh}.
}
\seealso{
  \code{\link{grob}} and
  \code{\link{gTree}}
}
\examples{
    grid.newpage()
    gt <- gTree(childrenvp=vpStack(
                  viewport(x=0, width=.5, just="left", name="vp"),
                  viewport(y=.5, height=.5, just="bottom", name="vp2")),
                children=gList(rectGrob(vp="vp::vp2", name="child")),
                name="parent")
    grid.draw(gt)
    showGrob()
    showGrob(gPath="child")
    showGrob(recurse=FALSE)
    showGrob(depth=1)
    showGrob(depth=2)
    showGrob(depth=1:2)
    showGrob(gt)
    showGrob(gt, gPath="child")
    showGrob(just="left", gp=gpar(col="red", cex=.5), rot=45)
    showGrob(labelfun=function(grob, ...) {
        x <- grobX(grob, "west")
        y <- grobY(grob, "north")
        gTree(children=gList(rectGrob(x=x, y=y,
                width=stringWidth(grob$name) + unit(2, "mm"),
                height=stringHeight(grob$name) + unit(2, "mm"),
                gp=gpar(col=NA, fill=rgb(1, 0, 0, .5)),
                just=c("left", "top")),
                textGrob(grob$name,
                         x=x + unit(1, "mm"), y=y - unit(1, "mm"),
                         just=c("left", "top"))))
    })

\dontrun{
    # Examples from higher-level packages

    library(lattice)
    # Ctrl-c after first example
    example(histogram)
    showGrob()
    showGrob(gPath="plot_01.ylab")

    library(ggplot2)
    # Ctrl-c after first example
    example(qplot)
    showGrob()
    showGrob(recurse=FALSE)
    showGrob(gPath="panel-3-3")
    showGrob(gPath="axis.title", grep=TRUE)
    showGrob(depth=2)
}
}
\keyword{ dplot }
