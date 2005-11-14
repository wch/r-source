
######################################
# move-to and line-to primitives
######################################
validDetails.move.to <- function(x) {
  if (!is.unit(x$x) ||
      !is.unit(x$y))
    stop("'x' and 'y' must be units")
  # Make sure that x and y are of length 1
  if (unit.length(x$x) > 1 | unit.length(x$y) > 1)
    stop("'x' and 'y' must have length 1")
  x
}

drawDetails.move.to <- function(x, recording=TRUE) {
  grid.Call.graphics("L_moveTo", x$x, x$y)
}

moveToGrob <- function(x=0, y=0,
                       default.units="npc",
                       name=NULL, vp=NULL) {
  if (!is.unit(x))
    x <- unit(x, default.units)
  if (!is.unit(y))
    y <- unit(y, default.units)
  grob(x=x, y=y,
       name=name, vp=vp, cl="move.to")
}

grid.move.to <- function(x=0, y=0,
                         default.units="npc",
                         name=NULL, draw=TRUE, vp=NULL) {
  mtg <- moveToGrob(x=x, y=y, default.units=default.units,
                    name=name, vp=vp)
  if (draw)
    grid.draw(mtg)
  invisible(mtg)
}

validDetails.line.to <- function(x) {
  if (!is.unit(x$x) ||
      !is.unit(x$y))
    stop("'x' and 'y' must be units")
  # Make sure that x and y are of length 1
  if (unit.length(x$x) > 1 | unit.length(x$y) > 1)
    stop("'x' and 'y' must have length 1")
  x
}

drawDetails.line.to <- function(x, recording=TRUE) {
  grid.Call.graphics("L_lineTo", x$x, x$y)
}

lineToGrob <- function(x=1, y=1,
                       default.units="npc",
                       name=NULL, gp=gpar(), vp=NULL) {
  if (!is.unit(x))
    x <- unit(x, default.units)
  if (!is.unit(y))
    y <- unit(y, default.units)
  grob(x=x, y=y,
       name=name, gp=gp, vp=vp, cl="line.to")
}

grid.line.to <- function(x=1, y=1,
                         default.units="npc",
                         name=NULL, gp=gpar(), draw=TRUE, vp=NULL) {
  ltg <- lineToGrob(x=x, y=y, default.units=default.units,
                    name=name, gp=gp, vp=vp)
  if (draw)
    grid.draw(ltg)
  invisible(ltg)
}

######################################
# LINES primitive
######################################
validDetails.lines <- function(x) {
  if (!is.unit(x$x) ||
      !is.unit(x$y))
    stop("'x' and 'y' must be units")
  x
}

drawDetails.lines <- function(x, recording=TRUE) {
  grid.Call.graphics("L_lines", x$x, x$y)
}

widthDetails.lines <- function(x) {
  bounds <- grid.Call("L_locnBounds", x$x, x$y)
  if (is.null(bounds))
    unit(0, "inches")
  else
    unit(bounds[2] - bounds[1], "inches")
}

heightDetails.lines <- function(x) {
  bounds <- grid.Call("L_locnBounds", x$x, x$y)
  if (is.null(bounds))
    unit(0, "inches")
  else
    unit(bounds[4] - bounds[3], "inches")
}

linesGrob <- function(x=unit(c(0, 1), "npc", units.per.obs),
                      y=unit(c(0, 1), "npc", units.per.obs),
                      default.units="npc", units.per.obs=FALSE,
                      name=NULL, gp=gpar(), vp=NULL) {
  # Allow user to specify unitless vector;  add default units
  if (!is.unit(x))
    x <- unit(x, default.units, units.per.obs)
  if (!is.unit(y))
    y <- unit(y, default.units, units.per.obs)
  grob(x=x, y=y, name=name, gp=gp, vp=vp, cl="lines")
}

# Specify "units.per.obs=TRUE" to give a unit or units per (x, y) pair
grid.lines <- function(x=unit(c(0, 1), "npc", units.per.obs),
                       y=unit(c(0, 1), "npc", units.per.obs),
                       default.units="npc", units.per.obs=FALSE,
                       name=NULL, gp=gpar(), draw=TRUE, vp=NULL) {
  lg <- linesGrob(x=x, y=y, default.units=default.units,
                  units.per.obs=units.per.obs,
                  name=name, gp=gp, vp=vp)
  if (draw)
    grid.draw(lg)
  invisible(lg)
}

######################################
# SEGMENTS primitive
######################################
validDetails.segments <- function(x) {
  if (!is.unit(x$x0) || !is.unit(x$x1) ||
      !is.unit(x$y0) || !is.unit(x$y1))
    stop("'x0', 'y0', 'x1', and 'y1' must be units")
  x
}

drawDetails.segments <- function(x, recording=TRUE) {
  grid.Call.graphics("L_segments", x$x0, x$y0, x$x1, x$y1)
}

widthDetails.segments <- function(x) {
  bounds <- grid.Call("L_locnBounds",
                      unit.c(x$x0, x$x1),
                      unit.c(x$y0, x$y1))
  if (is.null(bounds))
    unit(0, "inches")
  else
    unit(bounds[2] - bounds[1], "inches")
}

heightDetails.segments <- function(x) {
  bounds <- grid.Call("L_locnBounds",
                      unit.c(x$x0, x$x1),
                      unit.c(x$y0, x$y1))
  if (is.null(bounds))
    unit(0, "inches")
  else
    unit(bounds[4] - bounds[3], "inches")
}

# Specify "units.per.obs=TRUE" to give a unit or units per (x, y) pair
segmentsGrob <- function(x0=unit(0, "npc"), y0=unit(0, "npc"),
                         x1=unit(1, "npc"), y1=unit(1, "npc"),
                         default.units="npc", units.per.obs=FALSE,
                         name=NULL, gp=gpar(), vp=NULL) {
  # Allow user to specify unitless vector;  add default units
  if (!is.unit(x0))
    x0 <- unit(x0, default.units, units.per.obs)
  if (!is.unit(x1))
    x1 <- unit(x1, default.units, units.per.obs)
  if (!is.unit(y0))
    y0 <- unit(y0, default.units, units.per.obs)
  if (!is.unit(y1))
    y1 <- unit(y1, default.units, units.per.obs)
  grob(x0=x0, y0=y0, x1=x1, y1=y1, name=name, gp=gp, vp=vp,
       cl="segments")
}

grid.segments <- function(x0=unit(0, "npc"), y0=unit(0, "npc"),
                      x1=unit(1, "npc"), y1=unit(1, "npc"),
                      default.units="npc", units.per.obs=FALSE,
                      name=NULL, gp=gpar(), draw=TRUE, vp=NULL) {
  sg <- segmentsGrob(x0=x0, y0=y0, x1=x1, y1=y1,
                     default.units=default.units,
                     units.per.obs=units.per.obs,
                     name=name, gp=gp, vp=vp)
  if (draw)
    grid.draw(sg)
  invisible(sg)
}

######################################
# ARROWS primitive
######################################

validDetails.arrows <- function(x) {
  if ((!is.null(x$x) && !is.unit(x$x)) ||
      (!is.null(x$y) && !is.unit(x$y)))
    stop("'x' and 'y' must be units or NULL")
  if (!is.unit(x$length))
    stop("'length' must be a unit object")
  x$ends <- as.integer(match(x$ends, c("first", "last", "both")))
  x$type <- as.integer(match(x$type, c("open", "closed")))
  if (any(is.na(x$ends)) || any(is.na(x$type)))
    stop("Invalid 'ends' or 'type' argument")
  x
}

drawDetails.arrows <- function(x, recording=TRUE) {
  if (is.null(x$x)) { # y should be null too
    if (!is.null(x$y))
      stop("Corrupt 'arrows' object")
    lineThing <- getGrob(x, childNames(x))
    # This could be done via method dispatch, but that really
    # seemed like overkill
    # OTOH, this is NOT user-extensible
    # AND the code for, e.g., "lines" is not located with
    # the other grid.lines code so changes there are unlikely
    # to propagate to here (e.g., add an id arg to grid.lines?
    if (inherits(lineThing, "line.to")) {
      x1 <- NULL
      x2 <- lineThing$x
      y1 <- NULL
      y2 <- lineThing$y
      xnm1 <- NULL
      xn <- lineThing$x
      ynm1 <- NULL
      yn <- lineThing$y
    } else if (inherits(lineThing, "lines")) {
      # x or y may be recycled
      n <- max(unit.length(lineThing$x),
               unit.length(lineThing$y))
      xx <- unit.rep(lineThing$x, length=2)
      x1 <- xx[1]
      x2 <- xx[2]
      xx <- unit.rep(lineThing$x, length=n)
      xnm1 <- xx[n - 1]
      xn <- xx[n]
      yy <- unit.rep(lineThing$y, length=2)
      y1 <- yy[1]
      y2 <- yy[2]
      yy <- unit.rep(lineThing$y, length=n)
      ynm1 <- yy[n - 1]
      yn <- yy[n]
    } else { # inherits(lineThing, "segments")
      x1 <- lineThing$x0
      x2 <- lineThing$x1
      xnm1 <- lineThing$x0
      xn <- lineThing$x1
      y1 <- lineThing$y0
      y2 <- lineThing$y1
      ynm1 <- lineThing$y0
      yn <- lineThing$y1
    }
  } else {
    # x or y may be recycled
    n <- max(unit.length(x$x), unit.length(x$y))
    xx <- unit.rep(x$x, length=2)
    x1 <- xx[1]
    x2 <- xx[2]
    xx <- unit.rep(x$x, length=n)
    xnm1 <- xx[n - 1]
    xn <- xx[n]
    yy <- unit.rep(x$y, length=2)
    y1 <- yy[1]
    y2 <- yy[2]
    yy <- unit.rep(x$y, length=n)
    ynm1 <- yy[n - 1]
    yn <- yy[n]
    grid.Call.graphics("L_lines", x$x, x$y)
  }
  grid.Call.graphics("L_arrows", x1, x2, xnm1, xn, y1, y2, ynm1, yn,
                     x$angle, x$length, x$ends, x$type)
}

widthDetails.arrows <- function(x) {
  if (is.null(x$x)) { # y should be null too
    if (!is.null(x$y))
      stop("Corrupt 'arrows' object")
    lineThing <- getGrob(x, childNames(x))
    widthDetails(lineThing)
  } else {
    bounds <- grid.Call("L_locnBounds", x$x, x$y)
    if (is.null(bounds))
      unit(0, "inches")
    else
      unit(bounds[2] - bounds[1], "inches")
  }
}

heightDetails.arrows <- function(x) {
  if (is.null(x$x)) { # y should be null too
    if (!is.null(x$y))
      stop("Corrupt 'arrows' object")
    lineThing <- getGrob(x, childNames(x))
    heightDetails(lineThing)
  } else {
    bounds <- grid.Call("L_locnBounds", x$x, x$y)
    if (is.null(bounds))
      unit(0, "inches")
    else
      unit(bounds[4] - bounds[3], "inches")
  }
}

arrowsGrob <- function(x=c(0.25, 0.75), y=0.5,
                        default.units="npc",
                        grob=NULL,
                        angle=30, length=unit(0.25, "inches"),
                        ends="last", type="open",
                        name=NULL, gp=gpar(), vp=NULL) {
  if (is.null(grob)) {
    if (!is.unit(x))
      x <- unit(x, default.units)
    if (!is.unit(y))
      y <- unit(y, default.units)
  }
  # Check the grob here
  # Not in validDetails.arrows because that is for checking
  # slots of an arrows object (the grob is a child of the arrows object)
  # A possible alternative design would have a copy of the grob
  # stored in a slot of the arrows object;  then it could be checked
  # in the validDetails AND it could be edited
  if (!is.null(grob)) {
    # The grob can only be a "lines" or "segments"
    # (splines would be another candidate if they existed)
    if (!(inherits(grob, "lines") ||
          inherits(grob, "segments") ||
          inherits(grob, "line.to")))
      stop("The 'grob' argument must be a 'line.to', 'lines', or 'segments' grob")
    x <- y <- NULL
  }
  gTree(x=x, y=y, children=if (is.null(grob)) NULL else gList(grob),
       angle=as.numeric(angle), length=length,
       ends=ends, type=type,
       name=name, gp=gp, vp=vp, cl="arrows")
}

grid.arrows <- function(x=c(0.25, 0.75), y=0.5,
                        default.units="npc",
                        grob=NULL,
                        angle=30, length=unit(0.25, "inches"),
                        ends="last", type="open",
                        name=NULL, gp=gpar(), draw=TRUE, vp=NULL) {
  ag <- arrowsGrob(x=x, y=y,
                   default.units=default.units,
                   grob=grob, angle=angle, length=length,
                   ends=ends, type=type,
                   name=name, gp=gp, vp=vp)
  if (draw)
    grid.draw(ag)
  invisible(ag)
}

######################################
# POLYGON primitive
######################################

validDetails.polygon <- function(x) {
  if (!is.unit(x$x) ||
      !is.unit(x$y))
    stop("'x' and 'y' must be units")
  if (!is.null(x$id) && !is.null(x$id.lengths))
    stop("It is invalid to specify both 'id' and 'id.lengths")
  if (unit.length(x$x) != unit.length(x$y))
    stop("'x' and 'y' must be same length")
  if (!is.null(x$id) && (length(x$id) != unit.length(x$x)))
    stop("'x' and 'y' and 'id' must all be same length")
  if (!is.null(x$id))
    x$id <- as.integer(x$id)
  if (!is.null(x$id.lengths) && (sum(x$id.lengths) != unit.length(x$x)))
    stop("'x' and 'y' and 'id.lengths' must specify same overall length")
  if (!is.null(x$id.lengths))
    x$id.lengths <- as.integer(x$id.lengths)
  x
}

drawDetails.polygon <- function(x, recording=TRUE) {
  if (is.null(x$id) && is.null(x$id.lengths))
    grid.Call.graphics("L_polygon", x$x, x$y,
                       list(as.integer(1:length(x$x))))
  else {
    if (is.null(x$id)) {
      n <- length(x$id.lengths)
      id <- rep(1:n, x$id.lengths)
    } else {
      n <- length(unique(x$id))
      id <- x$id
    }
    index <- vector("list", n)
    count <- 1
    for (i in unique(id)) {
      index[[count]] <- as.integer((1:length(x$x))[id == i])
      count <- count + 1
    }
    grid.Call.graphics("L_polygon", x$x, x$y, index)
  }
}

widthDetails.polygon <- function(x) {
  bounds <- grid.Call("L_locnBounds", x$x, x$y)
  if (is.null(bounds))
    unit(0, "inches")
  else
    unit(bounds[2] - bounds[1], "inches")
}

heightDetails.polygon <- function(x) {
  bounds <- grid.Call("L_locnBounds", x$x, x$y)
  if (is.null(bounds))
    unit(0, "inches")
  else
    unit(bounds[4] - bounds[3], "inches")
}

polygonGrob <- function(x=c(0, 0.5, 1, 0.5), y=c(0.5, 1, 0.5, 0),
                        id=NULL, id.lengths=NULL,
                        default.units="npc",
                        name=NULL, gp=gpar(), vp=NULL) {
  if (!is.unit(x))
    x <- unit(x, default.units)
  if (!is.unit(y))
    y <- unit(y, default.units)
  grob(x=x, y=y, id=id,
       id.lengths=id.lengths,
       name=name, gp=gp, vp=vp, cl="polygon")
}

grid.polygon <- function(x=c(0, 0.5, 1, 0.5), y=c(0.5, 1, 0.5, 0),
                         id=NULL, id.lengths=NULL,
                         default.units="npc",
                         name=NULL, gp=gpar(), draw=TRUE, vp=NULL) {
  pg <- polygonGrob(x=x, y=y, id=id, id.lengths=id.lengths,
                    default.units=default.units,
                    name=name, gp=gp, vp=vp)
  if (draw)
    grid.draw(pg)
  invisible(pg)
}

######################################
# XSPLINE primitive
######################################

validDetails.xspline <- function(x) {
  if (!is.unit(x$x) ||
      !is.unit(x$y))
    stop("x and y must be units")
  if (!is.null(x$id) && !is.null(x$id.lengths))
    stop("It is invalid to specify both 'id' and 'id.lengths")
  if (unit.length(x$x) != unit.length(x$y))
    stop("'x' and 'y' must be same length")
  if (!is.null(x$id) && (length(x$id) != unit.length(x$x)))
    stop("'x' and 'y' and 'id' must all be same length")
  if (!is.null(x$id))
    x$id <- as.integer(x$id)
  if (!is.null(x$id.lengths) && (sum(x$id.lengths) != unit.length(x$x)))
    stop("'x' and 'y' and 'id.lengths' must specify same overall length")
  if (!is.null(x$id.lengths))
    x$id.lengths <- as.integer(x$id.lengths)
  if (any(x$shape < -1 || x$shape > 1))
    stop("shape must be between -1 and 1")
  x$open <- as.logical(x$open)
  if (x$open &&
      (x$shape[1] != 0 ||
       x$shape[length(x$shape)] != 0)) {
    warning("First and last shape set to 0")
    x$shape[c(1, length(x$shape))] <- 0
  }
  x
}

drawDetails.xspline <- function(x, recording=TRUE) {
  if (is.null(x$id) && is.null(x$id.lengths))
      grid.Call.graphics("L_xspline", x$x, x$y, x$shape, x$open, 
                         list(as.integer(1:length(x$x))))
  else {
    if (is.null(x$id)) {
      n <- length(x$id.lengths)
      id <- rep(1:n, x$id.lengths)
    } else {
      n <- length(unique(x$id))
      id <- x$id
    }
    index <- vector("list", n)
    count <- 1
    for (i in unique(id)) {
      index[[count]] <- as.integer((1:length(x$x))[id == i])
      count <- count + 1
    }
    grid.Call.graphics("L_xspline", x$x, x$y, x$shape, x$open, index)
  }
}

xsplineGrob <- function(x=c(0, 0.5, 1, 0.5), y=c(0.5, 1, 0.5, 0),
                        shape=0, open=TRUE,
                        id=NULL, id.lengths=NULL,
                        default.units="npc",
                        name=NULL, gp=gpar(), vp=NULL) {
  if (!is.unit(x))
    x <- unit(x, default.units)
  if (!is.unit(y))
    y <- unit(y, default.units)
  grob(x=x, y=y, shape=shape, open=open,
       id=id, id.lengths=id.lengths,
       name=name, gp=gp, vp=vp, cl="xspline")
}

grid.xspline <- function(...) {
  grid.draw(xsplineGrob(...))
}

######################################
# CIRCLE primitive
######################################

validDetails.circle <- function(x) {
  if (!is.unit(x$x) ||
      !is.unit(x$y) ||
      !is.unit(x$r))
    stop("'x', 'y', and 'r' must be units")
  x
}

drawDetails.circle <- function(x, recording=TRUE) {
  grid.Call.graphics("L_circle", x$x, x$y, x$r)
}

widthDetails.circle <- function(x) {
  bounds <- grid.Call("L_circleBounds", x$x, x$y, x$r)
  if (is.null(bounds))
    unit(0, "inches")
  else
    unit(bounds[2] - bounds[1], "inches")
}

heightDetails.circle <- function(x) {
  bounds <- grid.Call("L_circleBounds", x$x, x$y, x$r)
  if (is.null(bounds))
    unit(0, "inches")
  else
    unit(bounds[4] - bounds[3], "inches")
}

circleGrob <- function(x=0.5, y=0.5, r=0.5,
                       default.units="npc",
                       name=NULL, gp=gpar(), vp=NULL) {
  if (!is.unit(x))
    x <- unit(x, default.units)
  if (!is.unit(y))
    y <- unit(y, default.units)
  if (!is.unit(r))
    r <- unit(r, default.units)
  grob(x=x, y=y, r=r, name=name, gp=gp, vp=vp, cl="circle")
}

grid.circle <- function(x=0.5, y=0.5, r=0.5,
                        default.units="npc",
                        name=NULL, gp=gpar(), draw=TRUE, vp=NULL) {
  cg <- circleGrob(x=x, y=y, r=r,
                   default.units=default.units,
                   name=name, gp=gp, vp=vp)
  if (draw)
    grid.draw(cg)
  invisible(cg)
}

######################################
# RECT primitive
######################################
validDetails.rect <- function(x) {
  if (!is.unit(x$x) ||
      !is.unit(x$y) ||
      !is.unit(x$width) ||
      !is.unit(x$height))
    stop("'x', 'y', 'width', and 'height' must be units")
  valid.just(x$just)
  if (!is.null(x$hjust))
    x$hjust <- as.numeric(x$hjust)
  if (!is.null(x$vjust))
    x$vjust <- as.numeric(x$vjust)
  x
}

drawDetails.rect <- function(x, recording=TRUE) {
  grid.Call.graphics("L_rect", x$x, x$y, x$width, x$height,
                     resolveHJust(x$just, x$hjust),
                     resolveVJust(x$just, x$vjust))
}

widthDetails.rect <- function(x) {
  bounds <- grid.Call("L_rectBounds", x$x, x$y, x$width, x$height,
                      resolveHJust(x$just, x$hjust),
                      resolveVJust(x$just, x$vjust))
  if (is.null(bounds))
    unit(0, "inches")
  else
    unit(bounds[2] - bounds[1], "inches")
}

heightDetails.rect <- function(x) {
  bounds <- grid.Call("L_rectBounds", x$x, x$y, x$width, x$height,
                      resolveHJust(x$just, x$hjust),
                      resolveVJust(x$just, x$vjust))
  if (is.null(bounds))
    unit(0, "inches")
  else
    unit(bounds[4] - bounds[3], "inches")
}

rectGrob <- function(x=unit(0.5, "npc"), y=unit(0.5, "npc"),
                     width=unit(1, "npc"), height=unit(1, "npc"),
                     just="centre", hjust=NULL, vjust=NULL,
                     default.units="npc",
                     name=NULL, gp=gpar(), vp=NULL) {
  if (!is.unit(x))
    x <- unit(x, default.units)
  if (!is.unit(y))
    y <- unit(y, default.units)
  if (!is.unit(width))
    width <- unit(width, default.units)
  if (!is.unit(height))
    height <- unit(height, default.units)
  grob(x=x, y=y, width=width, height=height, just=just,
       hjust=hjust, vjust=vjust,
       name=name, gp=gp, vp=vp, cl="rect")
}

grid.rect <- function(x=unit(0.5, "npc"), y=unit(0.5, "npc"),
                      width=unit(1, "npc"), height=unit(1, "npc"),
                     just="centre", hjust=NULL, vjust=NULL,
                     default.units="npc",
                      name=NULL, gp=gpar(), draw=TRUE, vp=NULL) {
  rg <- rectGrob(x=x, y=y, width=width, height=height, just=just,
                 hjust=hjust, vjust=vjust,
                 default.units=default.units,
                 name=name, gp=gp, vp=vp)
  if (draw)
    grid.draw(rg)
  invisible(rg)
}

######################################
# TEXT primitive
######################################
validDetails.text <- function(x) {
  if (!is.language(x$label))
    x$label <- as.character(x$label)
  if (!is.unit(x$x) ||
      !is.unit(x$y))
    stop("'x' and 'y' must be units")
  x$rot <- as.numeric(x$rot)
  if (!all(is.finite(x$rot)) || length(x$rot) == 0)
    stop("Invalid 'rot' value")
  valid.just(x$just)
  if (!is.null(x$hjust))
    x$hjust <- as.numeric(x$hjust)
  if (!is.null(x$vjust))
    x$vjust <- as.numeric(x$vjust)
  x$check.overlap <- as.logical(x$check.overlap)
  x
}

drawDetails.text <- function(x, recording=TRUE) {
  grid.Call.graphics("L_text", x$label, x$x, x$y,
                     resolveHJust(x$just, x$hjust),
                     resolveVJust(x$just, x$vjust),
                     x$rot, x$check.overlap)
}

widthDetails.text <- function(x) {
  bounds <- grid.Call("L_textBounds", x$label, x$x, x$y,
                      resolveHJust(x$just, x$hjust),
                      resolveVJust(x$just, x$vjust),
                      x$rot)
  if (is.null(bounds))
    unit(0, "inches")
  else
    unit(bounds[2] - bounds[1], "inches")
}

heightDetails.text <- function(x) {
  bounds <- grid.Call("L_textBounds", x$label, x$x, x$y,
                      resolveHJust(x$just, x$hjust),
                      resolveVJust(x$just, x$vjust),
                      x$rot)
  if (is.null(bounds))
    unit(0, "inches")
  else
    unit(bounds[4] - bounds[3], "inches")
}

textGrob <- function(label, x=unit(0.5, "npc"), y=unit(0.5, "npc"),
                     just="centre", hjust=NULL, vjust=NULL,
                     rot=0, check.overlap=FALSE,
                     default.units="npc",
                     name=NULL, gp=gpar(), vp=NULL) {
  if (!is.unit(x))
    x <- unit(x, default.units)
  if (!is.unit(y))
    y <- unit(y, default.units)
  grob(label=label, x=x, y=y, just=just, hjust=hjust, vjust=vjust,
       rot=rot, check.overlap=check.overlap,
       name=name, gp=gp, vp=vp, cl="text")
}

grid.text <- function(label, x=unit(0.5, "npc"), y=unit(0.5, "npc"),
                      just="centre", hjust=NULL, vjust=NULL,
                      rot=0, check.overlap=FALSE,
                      default.units="npc",
                      name=NULL, gp=gpar(), draw=TRUE, vp=NULL) {
  tg <- textGrob(label=label, x=x, y=y, just=just,
                 hjust=hjust, vjust=vjust, rot=rot,
                 check.overlap=check.overlap,
                 default.units=default.units,
                 name=name, gp=gp, vp=vp)
  if (draw)
    grid.draw(tg)
  invisible(tg)
}

######################################
# POINTS primitive
######################################
valid.pch <- function(pch) {
  if (length(pch) == 0)
    stop("zero-length 'pch'")
  if (is.null(pch))
    pch <- as.integer(1)
  else if (!is.character(pch))
    pch <- as.integer(pch)
  pch
}

validDetails.points <- function(x) {
  if (!is.unit(x$x) ||
      !is.unit(x$y) ||
      !is.unit(x$size))
    stop("'x', 'y' and 'size' must be units")
  if (unit.length(x$x) != unit.length(x$y))
    stop("'x' and 'y' must be unit objects and have the same length")
  x$pch <- valid.pch(x$pch)
  x
}

drawDetails.points <- function(x, recording=TRUE) {
  grid.Call.graphics("L_points", x$x, x$y, x$pch, x$size)
}

# FIXME:  does not take into account the size of the symbols
widthDetails.points <- function(x) {
  bounds <- grid.Call("L_locnBounds", x$x, x$y)
  if (is.null(bounds))
    unit(0, "inches")
  else
    unit(bounds[2] - bounds[1], "inches")
}

heightDetails.points <- function(x) {
  bounds <- grid.Call("L_locnBounds", x$x, x$y)
  if (is.null(bounds))
    unit(0, "inches")
  else
    unit(bounds[4] - bounds[3], "inches")
}

pointsGrob <- function(x=runif(10),
                       y=runif(10),
                       pch=1, size=unit(1, "char"),
                       default.units="native",
                       name=NULL, gp=gpar(), vp=NULL) {
  if (!is.unit(x))
    x <- unit(x, default.units)
  if (!is.unit(y))
    y <- unit(y, default.units)
  grob(x=x, y=y, pch=pch, size=size,
       name=name, gp=gp, vp=vp, cl="points")
}

grid.points <- function(x=runif(10),
                        y=runif(10),
                        pch=1, size=unit(1, "char"),
                        default.units="native",
                        name=NULL, gp=gpar(),
                        draw=TRUE, vp=NULL) {
  pg <- pointsGrob(x=x, y=y, pch=pch, size=size,
                   default.units=default.units,
                   name=name, gp=gp, vp=vp)
  if (draw)
    grid.draw(pg)
  invisible(pg)
}












