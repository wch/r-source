% File src/library/grid/man/grid-defunct.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{grid-defunct}
\alias{grid-defunct}
\title{Defunct Functions in Package \pkg{grid}}
%------ PLEASE: put \alias{.} here for  EACH !
\alias{grid.grob}
\alias{grid.arrows}
\alias{arrowsGrob}
\alias{grid.convert}
\alias{grid.convertX}
\alias{grid.convertY}
\alias{grid.convertWidth}
\alias{grid.convertHeight}
\alias{grid.collection}
\alias{push.viewport}
\alias{pop.viewport}
\alias{viewport.transform}
\alias{draw.details}
\alias{convertNative}
%
\description{
  The functions or variables listed here are no longer part of \pkg{grid}.
}
\usage{
grid.grob(list.struct, cl = NULL, draw = TRUE) 
grid.arrows(x = c(0.25, 0.75), y = 0.5, default.units = "npc",
            grob = NULL,
            angle = 30, length = unit(0.25, "inches"),
            ends = "last", type = "open", name = NULL,
            gp = gpar(), draw = TRUE, vp = NULL)

arrowsGrob(x = c(0.25, 0.75), y = 0.5, default.units = "npc",
           grob = NULL,
           angle = 30, length = unit(0.25, "inches"),
           ends = "last", type = "open", name = NULL,
           gp = gpar(), vp = NULL)
grid.convertX(x, unitTo, valueOnly = FALSE)
grid.convertY(x, unitTo, valueOnly = FALSE)
grid.convertWidth(x, unitTo, valueOnly = FALSE)
grid.convertHeight(x, unitTo, valueOnly = FALSE)
grid.convert(x, unitTo,
             axisFrom = "x", typeFrom = "location",
             axisTo = axisFrom, typeTo = typeFrom,
             valueOnly = FALSE)
grid.collection(..., gp=gpar(), draw=TRUE, vp=NULL)
push.viewport(..., recording=TRUE)
pop.viewport(n=1, recording=TRUE)
viewport.transform(vp)
draw.details(x, recording)
convertNative(unit, dimension="x", type="location")
}

\keyword{internal}
