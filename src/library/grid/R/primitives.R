######################################
# move-to and line-to primitives
######################################
draw.details.move.to <- function(x, x.wrapped, recording=TRUE) {
  grid.Call.graphics("L_moveTo", x$x, x$y)
}

grid.move.to <- function(x=0, y=0,
                         default.units="npc",
                         draw=TRUE, vp=NULL) {
  if (!is.unit(x))
    x <- unit(x, default.units)
  if (!is.unit(y))
    y <- unit(y, default.units)
  # Make sure that x and y are of length 1
  if (unit.length(x) > 1 | unit.length(y) > 1)
    stop("x and y must have length 1")
  grid.grob(list(x=x, y=y, vp=vp), "move.to", draw)
}

draw.details.line.to <- function(x, x.wrapped, recording=TRUE) {
  grid.Call.graphics("L_lineTo", x$x, x$y)
}

grid.line.to <- function(x=1, y=1,
                         default.units="npc",
                         draw=TRUE, gp=gpar(), vp=NULL) {
  if (!is.unit(x))
    x <- unit(x, default.units)
  if (!is.unit(y))
    y <- unit(y, default.units)
  # Make sure that x and y are of length 1
  if (unit.length(x) > 1 | unit.length(y) > 1)
    stop("x and y must have length 1")
  grid.grob(list(x=x, y=y, gp=gp, vp=vp), "line.to", draw)
}

######################################
# LINES primitive
######################################
draw.details.lines <- function(x, x.wrapped, recording=TRUE) {
  grid.Call.graphics("L_lines", x$x, x$y)
}

# Specify "units.per.obs=TRUE" to give a unit or units per (x, y) pair
grid.lines <- function(x=unit(c(0, 1), "npc", units.per.obs),
                   y=unit(c(0, 1), "npc", units.per.obs),
                   default.units="npc", units.per.obs=FALSE,
                   gp=gpar(), draw=TRUE, vp=NULL) {
  # Allow user to specify unitless vector;  add default units
  if (!is.unit(x))
    x <- unit(x, default.units, units.per.obs)
  if (!is.unit(y))
    y <- unit(y, default.units, units.per.obs)
  l <- list(x=x, y=y, gp=gp, vp=vp)
  cl <- "lines"
  grid.grob(l, cl, draw)
}

######################################
# SEGMENTS primitive
######################################
draw.details.segments <- function(x, x.wrapped, recording=TRUE) {
  grid.Call.graphics("L_segments", x$x0, x$y0, x$x1, x$y1)
}

# Specify "units.per.obs=TRUE" to give a unit or units per (x, y) pair
grid.segments <- function(x0=unit(0, "npc"), y0=unit(0, "npc"),
                      x1=unit(1, "npc"), y1=unit(1, "npc"),
                      default.units="npc", units.per.obs=FALSE,
                      gp=gpar(), draw=TRUE, vp=NULL) {
  # Allow user to specify unitless vector;  add default units
  if (!is.unit(x0))
    x0 <- unit(x0, default.units, units.per.obs)
  if (!is.unit(x1))
    x1 <- unit(x1, default.units, units.per.obs)
  if (!is.unit(y0))
    y0 <- unit(y0, default.units, units.per.obs)
  if (!is.unit(y1))
    y1 <- unit(y1, default.units, units.per.obs)
  s <- list(x0=x0, y0=y0, x1=x1, y1=y1, gp=gp, vp=vp)
  cl <- "segments"
  grid.grob(s, cl, draw)
}

######################################
# ARROWS primitive
######################################
draw.details.arrows <- function(x, x.wrapped, recording=TRUE) {
  if (is.null(x$x)) { # y should be null too
    if (!is.null(x$y))
      stop("Corrupt arrows object")
    list.struct <- get.value(x$grob)
#    cl <- class(list.struct)
    # This could be done via method dispatch, but that really
    # seemed like overkill
    # OTOH, this is NOT user-extensible
    # AND the code for, e.g., "lines" is not located with
    # the other grid.lines code so changes there are unlikely
    # to propagate to here (e.g., add an id arg to grid.lines?
    if (inherits(list.struct, "line.to")) {
      x1 <- NULL
      x2 <- list.struct$x
      y1 <- NULL
      y2 <- list.struct$y
      xnm1 <- NULL
      xn <- list.struct$x
      ynm1 <- NULL
      yn <- list.struct$y
    } else if (inherits(list.struct, "lines")) {
      # x or y may be recycled
      n <- max(unit.length(list.struct$x),
               unit.length(list.struct$y))
      xx <- unit.rep(list.struct$x, length=2)
      x1 <- xx[1]
      x2 <- xx[2]
      xx <- unit.rep(list.struct$x, length=n)
      xnm1 <- xx[n - 1]
      xn <- xx[n]
      yy <- unit.rep(list.struct$y, length=2)
      y1 <- yy[1]
      y2 <- yy[2]
      yy <- unit.rep(list.struct$y, length=n)
      ynm1 <- yy[n - 1]
      yn <- yy[n]
    } else { # inherits(list.struct, "segments")
      x1 <- list.struct$x0
      x2 <- list.struct$x1
      xnm1 <- list.struct$x0
      xn <- list.struct$x1
      y1 <- list.struct$y0
      y2 <- list.struct$y1
      ynm1 <- list.struct$y0
      yn <- list.struct$y1
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

grid.arrows <- function(x=c(0.25, 0.75), y=0.5,
                        default.units="npc",
                        grob=NULL,
                        angle=30, length=unit(0.25, "inches"),
                        ends="last", type="open",
                        gp=gpar(), draw=TRUE, vp=NULL) {
  # If grob is specified, that overrides any x and y values
  if (!is.null(grob)) {
    if (!is.grob(grob))
      stop("The grob argument must be of class grob")
    list.struct <- get.value(grob)
    cl <- class(list.struct)
    # The grob can only be a "lines" or "segments"
    # (splines would be another candidate if they existed)
    if (!(inherits(list.struct, "lines") ||
          inherits(list.struct, "segments") ||
          inherits(list.struct, "line.to")))
      stop("The grob argument must be a line.to, lines, or segments grob")
    x <- y <- NULL
  } else {
    if (!is.unit(x))
      x <- unit(x, default.units)
    if (!is.unit(y))
      y <- unit(y, default.units)
  }
  if (!is.unit(length))
    stop("Length must be a unit object")
  ends <- as.integer(match(ends, c("first", "last", "both")))
  type <- as.integer(match(type, c("open", "closed")))
  if (any(is.na(ends)) || any(is.na(type)))
    stop("Invalid ends or type argument")
  a <- list(x=x, y=y, grob=grob,
            angle=as.numeric(angle), length=length,
            ends=ends, type=type, gp=gp, vp=vp)
  cl <- "arrows"
  grid.grob(a, cl, draw)
}

######################################
# POLYGON primitive
######################################

draw.details.polygon <- function(x, x.wrapped, recording=TRUE) {
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

grid.polygon <- function(x=c(0, 0.5, 1, 0.5), y=c(0.5, 1, 0.5, 0),
                         id=NULL, id.lengths=NULL,
                         default.units="npc",
                         gp=gpar(),draw=TRUE, vp=NULL) {
  if (!missing(id) && !missing(id.lengths))
    stop("It is invalid to specify both id and id.lenths")
  if (!is.unit(x))
    x <- unit(x, default.units)
  if (!is.unit(y))
    y <- unit(y, default.units)
  if (unit.length(x) != unit.length(y))
    stop("x and y must be same length")
  if (!is.null(id) && (length(id) != unit.length(x)))
    stop("x and y and id must all be same length")
  if (!is.null(id))
    id <- as.integer(id)
  if (!is.null(id.lengths) && (sum(id.lengths) != unit.length(x)))
    stop("x and y and id.lengths must specify same overall length")
  if (!is.null(id.lengths))
    id.lengths <- as.integer(id.lengths)
  p <- list(x=x, y=y, id=id,
            id.lengths=id.lengths, gp=gp, vp=vp)
  cl <- "polygon"
  grid.grob(p, cl, draw)
}

######################################
# CIRCLE primitive
######################################

draw.details.circle <- function(x, x.wrapped, recording=TRUE) {
  grid.Call.graphics("L_circle", x$x, x$y, x$r)
}

grid.circle <- function(x=0.5, y=0.5, r=0.5,
                         default.units="npc",
                         gp=gpar(),draw=TRUE, vp=NULL) {
  if (!is.unit(x))
    x <- unit(x, default.units)
  if (!is.unit(y))
    y <- unit(y, default.units)
  if (!is.unit(r))
    r <- unit(r, default.units)
  c <- list(x=x, y=y, r=r, gp=gp, vp=vp)
  cl <- "circle"
  grid.grob(c, cl, draw)
}

######################################
# RECT primitive
######################################
draw.details.rect <- function(x, x.wrapped, recording=TRUE) {
  grid.Call.graphics("L_rect", x$x, x$y, x$width, x$height,
                     valid.just(x$just))
}

width.details.rect <- function(x) {
  absolute.size(x$width)
}

height.details.rect <- function(x) {
  absolute.size(x$height)
}

grid.rect <- function(x=unit(0.5, "npc"), y=unit(0.5, "npc"),
                      width=unit(1, "npc"), height=unit(1, "npc"),
                      just="centre", default.units="npc",
                      gp=gpar(),draw=TRUE, vp=NULL) {
  if (!is.unit(x))
    x <- unit(x, default.units)
  if (!is.unit(y))
    y <- unit(y, default.units)
  if (!is.unit(width))
    width <- unit(width, default.units)
  if (!is.unit(height))
    height <- unit(height, default.units)
  r <- list(x=x, y=y, width=width, height=height, just=just, gp=gp, vp=vp)
  cl <- "rect"
  grid.grob(r, cl, draw)
}

######################################
# TEXT primitive
######################################
draw.details.text <- function(x, x.wrapped, recording=TRUE) {
  # FIXME:  Need type checking for "rot" and "check.overlap"
  grid.Call.graphics("L_text", x$label, x$x, x$y,
                 valid.just(x$just), x$rot, x$check.overlap)
}

width.details.text <- function(x) {
  unit(1, "mystrwidth", data=x$label)
}

height.details.text <- function(x) {
  unit(1, "mystrheight", data=x$label)
}

grid.text <- function(label, x=unit(0.5, "npc"), y=unit(0.5, "npc"),
                  just="centre", rot=0, check.overlap=FALSE,
                  default.units="npc", gp=gpar(), draw=TRUE, vp=NULL) {
  if (!is.unit(x))
    x <- unit(x, default.units)
  if (!is.unit(y))
    y <- unit(y, default.units)
  if (!is.expression(label))
    label <- as.character(label)
  rot <- as.numeric(rot)
  if (!is.finite(rot))
    stop("Invalid rot value")
  txt <- list(label=label, x=x, y=y, gp=gp,
              just=just, rot=rot, check.overlap=check.overlap,
              vp=vp)
  cl <- "text"
  grid.grob(txt, cl, draw)
}

######################################
# POINTS primitive
######################################
draw.details.points <- function(x, x.wrapped, recording=TRUE) {
  grid.Call.graphics("L_points", x$x, x$y, x$pch, x$size)
}

valid.pch <- function(pch) {
  if (length(pch) == 0)
    stop("zero-length pch")
  if (is.null(pch))
    pch <- as.integer(1)
  else if (!is.character(pch))
    pch <- as.integer(pch)
  pch
}

grid.points <- function(x=runif(10),
                        y=runif(10),
                        pch=1, size=unit(1, "char"),
                        default.units="native", gp=gpar(),
                        draw=TRUE, vp=NULL) {
  if (!is.unit(x))
    x <- unit(x, default.units)
  if (!is.unit(y))
    y <- unit(y, default.units)
  if (unit.length(x) != unit.length(y))
    stop("x and y must be unit objects and have the same length")
  p <- list(x=x, y=y, pch=valid.pch(pch), size=size, gp=gp, vp=vp)
  cl <- "points"
  grid.grob(p, cl, draw)
}

