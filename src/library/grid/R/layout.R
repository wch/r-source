#  File src/library/grid/R/layout.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2012 The R Core Team
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/


is.layout <- function(l) {
  inherits(l, "layout")
}

# FIXME:  The internal C code now does a lot of recycling of
# unit values, units, and data.  Can some/most/all of the
# recycling stuff below be removed ?
valid.layout <- function(nrow, ncol, widths, heights, respect, just) {
  nrow <- as.integer(nrow)
  ncol <- as.integer(ncol)
  # make sure we're dealing with a unit object
  if (!is.logical(respect)) {
    respect <- as.matrix(respect)
    if (!is.matrix(respect) || any(dim(respect) != c(nrow, ncol)))
      stop("'respect' must be logical or an 'nrow' by 'ncol' matrix")
    }
  if (is.matrix(respect)) {
    respect.mat <- matrix(as.integer(respect),
                          dim(respect)[1L],
                          dim(respect)[2L])
    respect <- 2
  }
  else respect.mat <- matrix(0L, nrow, ncol)

  valid.just <- valid.just(just)
  l <- list(nrow = nrow, ncol = ncol,
            widths = widths, heights = heights,
            respect = respect, valid.respect=as.integer(respect),
            respect.mat = respect.mat,
            just=just, valid.just=valid.just)
  class(l) <- "layout"
  l
}

layout.torture <- function() {
  top.vp <- viewport(y=0, height=unit(1, "npc") - unit(1.5, "lines"),
                     just=c("centre", "bottom"))
  do.label <- function(label) {
    grid.rect(y=1, height=unit(1.5, "lines"),
              just=c("center", "top"))
    grid.text(label,
              y=unit(1, "npc") - unit(1, "lines"),
              gp=gpar(font=2))
  }
  # 1 = all relative widths and heights
  grid.show.layout(grid.layout(3,2), vp=top.vp)
  do.label("All dimensions relative -- no respect")
  # (1) with full respect
  grid.show.layout(grid.layout(3,2, respect=TRUE), vp=top.vp)
  do.label("All dimensions relative -- full respect")
  # (1) with partial respect
  grid.show.layout(grid.layout(3,2,respect=matrix(c(1,0,0,0,0,0), 3L, 2L, TRUE)),
                   vp=top.vp)
  do.label("All dimensions relative -- only top-left cell respected")
  # (1) with slightly weirder partial respect
  grid.show.layout(grid.layout(3,2,respect=matrix(c(1,0,0,0,0,1), 3L, 2L, TRUE)),
                   vp=top.vp)
  do.label("All relative -- top-left, bottom-right respected")
  # 2 = combination of absolute and relative widths and heights
  grid.show.layout(grid.layout(2, 3,
                       widths=unit(c(2,4,1), c("null", "cm", "null")),
                       heights=unit(c(6,4), c("cm", "null"))),
                   vp=top.vp)
  do.label("Absolute and relative -- no respect")
  # (2) with full respect
  grid.show.layout(grid.layout(2, 3,
                       widths=unit(c(2,4,1), c("null", "cm", "null")),
                       heights=unit(c(6,4), c("cm", "null")), respect=TRUE),
                   vp=top.vp)
  do.label("Absolute and relative -- full respect")
  # (2) with partial respect
  grid.show.layout(grid.layout(2, 3,
                       widths=unit(c(2,4,1), c("null", "cm", "null")),
                       heights=unit(c(6,4), c("cm", "null")),
                       respect=matrix(c(0,0,0,0,0,1), 2L, 3L, TRUE)),
                   vp=top.vp)
  do.label("Absolute and relative -- bottom-right respected")
}

# Return the region allocated by the layout of the current viewport
layoutRegion <- function(layout.pos.row=1, layout.pos.col=1) {
  region <- grid.Call(L_layoutRegion,
                      # This conversion matches the vailidity check in
                      # valid.viewport()
                      if (is.null(layout.pos.row)) layout.pos.row
                      else as.integer(rep(layout.pos.row, length.out=2)),
                      if (is.null(layout.pos.col)) layout.pos.col
                      else as.integer(rep(layout.pos.col, length.out=2)))
  list(left=unit(region[1L], "npc"),
       bottom=unit(region[2L], "npc"),
       width=unit(region[3L], "npc"),
       height=unit(region[4L], "npc"))
}

####################
# Accessors
####################

layout.nrow <- function(lay) {
  lay$nrow
}

layout.ncol <- function(lay) {
  lay$ncol
}

layout.widths <- function(lay) {
  lay$widths
}

layout.heights <- function(lay) {
  lay$heights
}

layout.respect <- function(lay) {
  switch(lay$respect + 1,
         FALSE,
         TRUE,
         lay$respect.mat)
}

####################
# Public constructor function
####################
grid.layout <- function (nrow = 1, ncol = 1,
                         widths = unit(rep(1, ncol), "null"),
                         heights = unit(rep(1, nrow), "null"),
                         default.units = "null",
                         respect = FALSE,
                         just="centre")
{
  if (!is.unit(widths))
    widths <- unit(widths, default.units)
  if (!is.unit(heights))
    heights <- unit(heights, default.units)
  valid.layout(nrow, ncol, widths, heights, respect, just)
}

####################
# Utility Functions
####################

dim.layout <- function(x) {
    c(x$nrow, x$ncol)
}
