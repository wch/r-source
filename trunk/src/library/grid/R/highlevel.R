#  File src/library/grid/R/highlevel.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2013 The R Core Team
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
#  https://www.R-project.org/Licenses/

######################################
## Example applications of grid    #
######################################

grid.strip <- function(label="whatever", range.full=c(0, 1),
                   range.thumb=c(.3, .6),
                   fill="#FFBF00", thumb="#FF8000",
                   vp=NULL) {
  diff.full <- diff(range.full)
  diff.thumb <- diff(range.thumb)
  if (!is.null(vp))
    pushViewport(vp)
  grid.rect(gp=gpar(col=NULL, fill=fill))
  grid.rect((range.thumb[1L] - range.full[1L])/diff.full, 0,
            diff.thumb/diff.full, 1,
            just=c("left", "bottom"),
            gp=gpar(col=NULL, fill=thumb))
  grid.text(as.character(label))
  if (!is.null(vp))
    popViewport()
}

grid.panel <- function(x = stats::runif(10), y = stats::runif(10),
                   zrange = c(0, 1), zbin = stats::runif(2),
                   xscale = extendrange(x),
                   yscale = extendrange(y),
                   axis.left = TRUE, axis.left.label = TRUE,
                   axis.right = FALSE, axis.right.label = TRUE,
                   axis.bottom = TRUE, axis.bottom.label = TRUE,
                   axis.top = FALSE, axis.top.label = TRUE,
                   vp=NULL) {
  if (!is.null(vp))
    pushViewport(vp)
  temp.vp <- viewport(layout=grid.layout(2, 1,
                         heights=unit(c(1, 1), c("lines", "null"))))
  pushViewport(temp.vp)
  strip.vp <- viewport(layout.pos.row=1, layout.pos.col=1,
                        xscale=xscale)
  pushViewport(strip.vp)
  grid.strip(range.full=zrange, range.thumb=zbin)
  grid.rect()
  if (axis.top)
    grid.xaxis(main=FALSE, label=axis.top.label)
  popViewport()
  plot.vp <- viewport(layout.pos.row=2, layout.pos.col=1,
                       xscale=xscale, yscale=yscale)
  pushViewport(plot.vp)
  grid.grill()
  grid.points(x, y, gp=gpar(col="blue"))
  grid.rect()
  if (axis.left)
    grid.yaxis(label=axis.left.label)
  if (axis.right)
    grid.yaxis(main=FALSE, label=axis.right.label)
  if (axis.bottom)
    grid.xaxis(label=axis.bottom.label)
  popViewport(2)
  if (!is.null(vp))
    popViewport()
  invisible(list(strip.vp = strip.vp, plot.vp = plot.vp))
}

grid.multipanel <- function(x = stats::runif(90), y = stats::runif(90),
                            z = stats::runif(90),
                            nplots = 9, nrow = 5, ncol = 2,
                            newpage = TRUE, vp = NULL)
{
    if (newpage)
        grid.newpage()
    if (!is.null(vp))
        pushViewport(vp)
    stopifnot(nplots >= 1)
    if((missing(nrow) || missing(ncol)) && !missing(nplots)) {
        ## determine 'smart' default ones
        rowcol <- grDevices::n2mfrow(nplots)
        nrow <- rowcol[1L]
        ncol <- rowcol[2L]
    }
    temp.vp <- viewport(layout = grid.layout(nrow, ncol))
    pushViewport(temp.vp)
    xscale <- extendrange(x)
    yscale <- extendrange(y)
    breaks <- seq.int(min(z), max(z), length.out = nplots + 1)
    for (i in 1L:nplots) {
        col <- (i - 1) %% ncol + 1
        row <- (i - 1) %/% ncol + 1
        panel.vp <- viewport(layout.pos.row = row,
                             layout.pos.col = col)
        panelx <- x[z >= breaks[i] & z <= breaks[i+1]]
        panely <- y[z >= breaks[i] & z <= breaks[i+1]]
        grid.panel(panelx, panely, range(z), c(breaks[i], breaks[i+1]),
                   xscale, yscale,
                   axis.left = (col == 1),
                   axis.right = (col == ncol || i == nplots),
                   axis.bottom = (row == nrow),
                   axis.top = (row == 1),
                   axis.left.label = is.even(row),
                   axis.right.label = is.odd(row),
                   axis.bottom.label = is.even(col),
                   axis.top.label = is.odd(col),
                   vp = panel.vp)
    }
    grid.text("Compression Ratio", unit(.5, "npc"), unit(-4, "lines"),
              gp = gpar(fontsize = 20),
              just = "center", rot = 0)
    grid.text("NOx (micrograms/J)", unit(-4, "lines"), unit(.5, "npc"),
              gp = gpar(fontsize = 20),
              just = "centre", rot = 90)
    popViewport()
    if (!is.null(vp))
        popViewport()
}

grid.show.layout <- function(l, newpage=TRUE, vp.ex=0.8,
                             bg="light grey",
                             cell.border="blue", cell.fill="light blue",
                             cell.label=TRUE, label.col="blue",
                             unit.col="red", vp=NULL, ...) {
  if (!is.layout(l))
    stop("'l' must be a layout")
  if (newpage)
    grid.newpage()
  if (!is.null(vp))
    pushViewport(vp)
  grid.rect(gp=gpar(col=NULL, fill=bg))
  vp.mid <- viewport(0.5, 0.5, vp.ex, vp.ex, layout=l)
  pushViewport(vp.mid)
  grid.rect(gp=gpar(fill="white"))
  gp.red <- gpar(col=unit.col)
  for (i in 1L:l$nrow)
    for (j in 1L:l$ncol) {
      vp.inner <- viewport(layout.pos.row=i, layout.pos.col=j)
      pushViewport(vp.inner)
      grid.rect(gp=gpar(col=cell.border, fill=cell.fill))
      if (cell.label)
        grid.text(paste0("(", i, ", ", j, ")"), gp=gpar(col=label.col))
      if (j==1)
        # recycle heights if necessary
        grid.text(format("["(l$heights, i, top=FALSE), ...), gp=gp.red,
              just=c("right", "centre"),
              x=unit(-.05, "inches"), y=unit(.5, "npc"), rot=0)
      if (i==l$nrow)
        # recycle widths if necessary
        grid.text(format("["(l$widths, j, top=FALSE), ...), gp=gp.red,
              just=c("centre", "top"),
              x=unit(.5, "npc"), y=unit(-.05, "inches"), rot=0)
      if (j==l$ncol)
        # recycle heights if necessary
        grid.text(format("["(l$heights, i, top=FALSE), ...), gp=gp.red,
              just=c("left", "centre"),
              x=unit(1, "npc") + unit(.05, "inches"), y=unit(.5, "npc"),
              rot=0)
      if (i==1)
        # recycle widths if necessary
        grid.text(format("["(l$widths, j, top=FALSE), ...), gp=gp.red,
              just=c("centre", "bottom"),
              x=unit(.5, "npc"), y=unit(1, "npc") + unit(.05, "inches"),
              rot=0)
      popViewport()
    }
  popViewport()
  if (!is.null(vp))
    popViewport()
  ## return the viewport used to represent the parent viewport
  invisible(vp.mid)
}

grid.show.viewport <- function(v, parent.layout=NULL, newpage=TRUE, vp.ex=0.8,
                               border.fill="light grey",
                               vp.col="blue", vp.fill="light blue",
                               scale.col="red",
                               vp=NULL)
{
    ## if the viewport has a non-NULL layout.pos.row or layout.pos.col
    ## AND the viewport has a parent AND the parent has a layout
    ## represent the location of the viewport in the parent's layout ...
    if ((!is.null(v$layout.pos.row) || !is.null(v$layout.pos.col)) &&
        !is.null(parent.layout)) {
        if (!is.null(vp))
            pushViewport(vp)
        vp.mid <- grid.show.layout(parent.layout, vp.ex=vp.ex,
                                   cell.border="grey", cell.fill="white",
                                   cell.label=FALSE, newpage=newpage)
        pushViewport(vp.mid)
        pushViewport(v)
        gp.red <- gpar(col=scale.col)
        grid.rect(gp=gpar(col="blue", fill="light blue"))
        at <- grid.pretty(v$xscale)
        grid.xaxis(at=c(min(at), max(at)), gp=gp.red)
        at <- grid.pretty(v$yscale)
        grid.yaxis(at=c(min(at), max(at)), gp=gp.red)
        popViewport(2)
        if (!is.null(vp))
            popViewport()
    } else {
        if (newpage)
            grid.newpage()
        if (!is.null(vp))
            pushViewport(vp)
        grid.rect(gp=gpar(col=NULL, fill=border.fill))
        ## generate a viewport within the "top" viewport (vp) to represent the
        ## parent viewport of the viewport we are "show"ing (v).
        ## This is so that annotations at the edges of the
        ## parent viewport will be at least partially visible
        vp.mid <- viewport(0.5, 0.5, vp.ex, vp.ex)
        pushViewport(vp.mid)
        grid.rect(gp=gpar(fill="white"))
        x <- v$x
        y <- v$y
        w <- v$width
        h <- v$height
        pushViewport(v)
        grid.rect(gp=gpar(col=vp.col, fill=vp.fill))
        ## represent the "native" scale
        gp.red <- gpar(col=scale.col)
        at <- grid.pretty(v$xscale)
        grid.xaxis(at=c(min(at), max(at)), gp=gp.red)
        at <- grid.pretty(v$yscale)
        grid.yaxis(at=c(min(at), max(at)), gp=gp.red)
        grid.text(as.character(w), gp=gp.red,
                  just=c("centre", "bottom"),
                  x=unit(.5, "npc"), y=unit(1, "npc") + unit(.05, "inches"))
        grid.text(as.character(h), gp=gp.red,
                  just=c("left", "centre"),
                  x=unit(1, "npc") + unit(.05, "inches"), y=unit(.5, "npc"))
        popViewport()
        ## annotate the location and dimensions of the viewport
        grid.lines(unit.c(x, x), unit.c(unit(0, "npc"), y),
                   gp=gpar(col=scale.col, lty="dashed"))
        grid.lines(unit.c(unit(0, "npc"), x), unit.c(y, y),
                   gp=gpar(col=scale.col, lty="dashed"))
        grid.text(as.character(x), gp=gp.red,
                  just=c("centre", "top"),
                  x=x, y=unit(-.05, "inches"))
        grid.text(as.character(y), gp=gp.red,
                  just=c("bottom"),
                  x=unit(-.05, "inches"), y=y, rot=90)
        popViewport()
        if (!is.null(vp))
            popViewport()
    }
}

## old grid.legend <-
function(pch, labels, frame=TRUE,
                        hgap=unit(0.5, "lines"), vgap=unit(0.5, "lines"),
                        default.units="lines",
                        gp=gpar(), draw=TRUE,
                        vp=NULL) {
  ## Type checking on arguments
  labels <- as.character(labels)
  nkeys <- length(labels)
  if (length(pch) != nkeys)
    stop("'pch' and 'labels' not the same length")
  if (!is.unit(hgap))
    hgap <- unit(hgap, default.units)
  if (length(hgap) != 1)
    stop("'hgap' must be single unit")
  if (!is.unit(vgap))
    vgap <- unit(vgap, default.units)
  if (length(vgap) != 1)
    stop("'vgap' must be single unit")
  gf <- grid.frame(layout=grid.layout(nkeys, 2), vp=vp, gp=gp, draw=FALSE)
  for (i in 1L:nkeys) {
    if (i==1) {
      symbol.border <- unit.c(vgap, hgap, vgap, hgap)
      text.border <- unit.c(vgap, unit(0, "npc"), vgap, hgap)
    }
    else {
      symbol.border <- unit.c(vgap, hgap, unit(0, "npc"), hgap)
      text.border <- unit.c(vgap, unit(0, "npc"), unit(0, "npc"), hgap)
    }
    grid.pack(gf, grid.points(.5, .5, pch=pch[i], draw=FALSE),
              col=1, row=i, border=symbol.border,
              width=unit(1, "lines"), height=unit(1, "lines"),
              force.width=TRUE, draw=FALSE)
    grid.pack(gf, grid.text(labels[i], x=0, y=.5, just=c("left", "centre"),
                            draw=FALSE),
              col=2, row=i, border=text.border, draw=FALSE)
  }
  if (draw)
    grid.draw(gf)
  gf
}


legendGrob <-
    function(labels, nrow, ncol, byrow=FALSE,
	     do.lines = has.lty || has.lwd, lines.first=TRUE,
	     hgap=unit(1, "lines"), vgap=unit(1, "lines"),
	     default.units="lines",
	     pch, gp=gpar(), vp=NULL)
{
    ## Type checking on arguments; labels: character, symbol or expression:
    labels <- as.graphicsAnnot(labels)
    labels <- if(is.character(labels)) as.list(labels) else as.expression(labels)
    nkeys <- if(is.call(labels)) 1 else length(labels)
    if(nkeys == 0) return(nullGrob(vp=vp))
    if (!is.unit(hgap))
	hgap <- unit(hgap, default.units)
    if (length(hgap) != 1) stop("'hgap' must be single unit")
    if (!is.unit(vgap))
	vgap <- unit(vgap, default.units)
    if (length(vgap) != 1) stop("'vgap' must be single unit")
    ## nrow, ncol
    miss.nrow <- missing(nrow)
    miss.ncol <- missing(ncol)
    if(miss.nrow && miss.ncol) {ncol <- 1; nrow <- nkeys} # defaults to 1-column legend
    else if( miss.nrow && !miss.ncol) nrow <- ceiling(nkeys / ncol)
    else if(!miss.nrow &&  miss.ncol) ncol <- ceiling(nkeys / nrow)
    if(nrow < 1) stop("'nrow' must be >= 1")
    if(ncol < 1) stop("'ncol' must be >= 1")
    if(nrow * ncol < nkeys)
        stop("nrow * ncol < #{legend labels}")
    ## pch, gp
    if(has.pch <- !missing(pch) && length(pch) > 0) pch <- rep_len(pch, nkeys)
    if(doGP <- length(nmgp <- names(gp)) > 0) {
	if(has.lty  <-  "lty" %in% nmgp) gp$lty  <- rep_len(gp$lty, nkeys)
	if(has.lwd  <-  "lwd" %in% nmgp) gp$lwd  <- rep_len(gp$lwd, nkeys)
	if(has.col  <-  "col" %in% nmgp) gp$col  <- rep_len(gp$col,  nkeys)
	if(has.fill <- "fill" %in% nmgp) gp$fill <- rep_len(gp$fill, nkeys)
    } else {
	gpi <- gp
	if(missing(do.lines)) do.lines <- FALSE
    }

    ## main
    u0 <- unit(0, "npc")
    u1 <- unit(1, "char")
    ord <- if(lines.first) 1:2 else 2:1
    fg <- frameGrob(vp = vp)	  # set up basic frame grob (for packing)
    for (i in seq_len(nkeys)) {
	if(doGP) {
	    gpi <- gp
	    if(has.lty)	 gpi$lty <- gp$lty[i]
	    if(has.lwd)	 gpi$lwd <- gp$lwd[i]
	    if(has.col)	 gpi$col <- gp$col[i]
	    if(has.fill) gpi$fill<- gp$fill[i]
	}
	if(byrow) {
	    ci <- 1+ (i-1) %%  ncol
	    ri <- 1+ (i-1) %/% ncol
	} else {
	    ci <- 1+ (i-1) %/% nrow
	    ri <- 1+ (i-1) %%  nrow
	}
	## borders; unit.c creates a 4-vector of borders (bottom, left, top, right)
	vg <- if(ri != nrow) vgap else u0
	symbol.border <- unit.c(vg, u0, u0, 0.5 * hgap)
	text.border   <- unit.c(vg, u0, u0, if(ci != ncol) hgap else u0)

	## points/lines grob:
	plGrob <- if(has.pch && do.lines)
	    gTree(children = gList(linesGrob (0:1, 0.5, gp=gpi),
		  pointsGrob(0.5, 0.5, default.units="npc", pch=pch[i], gp=gpi))[ord])
	else if(has.pch) pointsGrob(0.5, 0.5, default.units="npc", pch=pch[i], gp=gpi)
	else if(do.lines) linesGrob(0:1, 0.5, gp=gpi)
	else nullGrob() # should not happen...
	fg <- packGrob(fg, plGrob,
		       col = 2*ci-1, row = ri, border = symbol.border,
		       width = u1, height = u1, force.width = TRUE)
	## text grob: add the labels
	gpi. <- gpi
	gpi.$col <- "black" # maybe needs its own 'gp' in the long run (?)
	fg <- packGrob(fg, textGrob(labels[[i]], x = 0, y = 0.5,
				    just = c("left", "centre"), gp=gpi.),
		       col = 2*ci, row = ri, border = text.border)
    }
    fg
}

grid.legend <- function(..., draw=TRUE)
{
    g <- legendGrob(...)# will error out if '...' has nonsense
    if (draw)
	grid.draw(g)
    invisible(g)
}

## Just a wrapper for a sample series of grid commands
grid.plot.and.legend <- function() {
  grid.newpage()
  top.vp <- viewport(width=0.8, height=0.8)
  pushViewport(top.vp)
  x <- stats::runif(10)
  y1 <- stats::runif(10)
  y2 <- stats::runif(10)
  pch <- 1L:3
  labels <- c("Girls", "Boys", "Other")
  lf <- frameGrob()
  plot <- gTree(children=gList(rectGrob(),
                  pointsGrob(x, y1, pch=1),
                  pointsGrob(x, y2, pch=2),
                  xaxisGrob(),
                  yaxisGrob()))
  lf <- packGrob(lf, plot)
  lf <- packGrob(lf, grid.legend(labels, pch=pch, draw=FALSE),
                 height=unit(1,"null"), side="right")
  grid.draw(lf)
}

