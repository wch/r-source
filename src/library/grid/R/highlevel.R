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
  grid.rect((range.thumb[1] - range.full[1])/diff.full, 0,
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
        nrow <- rowcol[1]
        ncol <- rowcol[2]
    }
    temp.vp <- viewport(layout = grid.layout(nrow, ncol))
    pushViewport(temp.vp)
    xscale <- extendrange(x)
    yscale <- extendrange(y)
    breaks <- seq(min(z), max(z), length = nplots + 1)
    for (i in 1:nplots) {
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

grid.show.layout <- function(l, newpage=TRUE,
                             bg="light grey",
                         cell.border="blue", cell.fill="light blue",
                         cell.label=TRUE, label.col="blue",
                             unit.col="red", vp=NULL) {
  if (!is.layout(l))
    stop("'l' must be a layout")
  if (newpage)
    grid.newpage()
  if (!is.null(vp))
    pushViewport(vp)
  grid.rect(gp=gpar(col=NULL, fill=bg))
  vp.mid <- viewport(0.5, 0.5, 0.8, 0.8, layout=l)
  pushViewport(vp.mid)
  grid.rect(gp=gpar(fill="white"))
  gp.red <- gpar(col=unit.col)
  for (i in 1:l$nrow)
    for (j in 1:l$ncol) {
      vp.inner <- viewport(layout.pos.row=i, layout.pos.col=j)
      pushViewport(vp.inner)
      grid.rect(gp=gpar(col=cell.border, fill=cell.fill))
      if (cell.label)
        grid.text(paste("(", i, ", ", j, ")", sep=""), gp=gpar(col=label.col))
      if (j==1)
        grid.text(as.character(l$heights[i]), gp=gp.red,
              just=c("right", "centre"),
              x=unit(-.05, "inches"), y=unit(.5, "npc"), rot=0)
      if (i==l$nrow)
        grid.text(as.character(l$widths[j]), gp=gp.red,
              just=c("centre", "top"),
              x=unit(.5, "npc"), y=unit(-.05, "inches"), rot=0)
      if (j==l$ncol)
        grid.text(as.character(l$heights[i]), gp=gp.red,
              just=c("left", "centre"),
              x=unit(1, "npc") + unit(.05, "inches"), y=unit(.5, "npc"),
              rot=0)
      if (i==1)
        grid.text(as.character(l$widths[j]), gp=gp.red,
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

grid.show.viewport <- function(v, parent.layout=NULL, newpage=TRUE,
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
        vp.mid <- grid.show.layout(parent.layout,
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
        vp.mid <- viewport(0.5, 0.5, 0.8, 0.8)
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
  for (i in 1:nkeys) {
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

grid.legend <-
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
  legend.layout <-
    grid.layout(nkeys, 3,
                widths=unit.c(unit(2, "lines"),
                  max(unit(rep(1, nkeys), "strwidth", as.list(labels))),
                  hgap),
                heights=unit.pmax(unit(2, "lines"),
                  vgap + unit(rep(1, nkeys), "strheight", as.list(labels))))
  fg <- frameGrob(layout=legend.layout, vp=vp, gp=gp)
  for (i in 1:nkeys) {
    fg <- placeGrob(fg, pointsGrob(.5, .5, pch=pch[i]), col=1, row=i)
    fg <- placeGrob(fg, textGrob(labels[i], x=0, y=.5,
                                 just=c("left", "centre")),
                    col=2, row=i)
  }
  if (draw)
    grid.draw(fg)
  fg
}

## Just a wrapper for a sample series of grid commands
grid.plot.and.legend <- function() {
  grid.newpage()
  top.vp <- viewport(w=0.8, h=0.8)
  pushViewport(top.vp)
  x <- stats::runif(10)
  y1 <- stats::runif(10)
  y2 <- stats::runif(10)
  pch <- 1:3
  labels <- c("Girls", "Boys", "Other")
  lf <- frameGrob()
  plot <- gTree(children=gList(rectGrob(),
                  pointsGrob(x, y1, pch=1),
                  pointsGrob(x, y2, pch=2),
                  xaxisGrob(),
                  yaxisGrob()))
  lf <- packGrob(lf, plot)
  lf <- packGrob(lf, grid.legend(pch, labels, draw=FALSE),
                 height=unit(1,"null"), side="right")
  grid.draw(lf)
}

