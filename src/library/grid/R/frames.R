################
# frame class
################
# NOTE: make framevp separate slot (rather than combining with
# normal vp slot) so that it can be edited (e.g., by grid.pack)
frameGrob <- function(layout=NULL, name=NULL, gp=gpar(), vp=NULL) {
  if (!is.null(layout)) 
    framevp <- viewport(layout=layout)
  else 
    framevp <- NULL
  gTree(framevp=framevp, name=name, gp=gp, vp=vp,
        cl="frame")
}

# draw=TRUE will not draw anything, but will mean that
# additions to the frame are drawn
grid.frame <- function(layout=NULL, name=NULL, gp=gpar(), vp=NULL,
                       draw=TRUE) {
  fg <- frameGrob(layout=layout, name=name, gp=gp, vp=vp)
  if (draw)
    grid.draw(fg)
  invisible(fg)
}

preDrawDetails.frame <- function(x) {
  if (!is.null(x$framevp))
    pushViewport(x$framevp, recording=FALSE)
}

postDrawDetails.frame <- function(x) {
  if (!is.null(x$framevp))
    upViewport(recording=FALSE)
}

widthDetails.frame <- function(x) {
  if (is.null(x$framevp))
    unit(1, "null")
  else
    sum(layout.widths(viewport.layout(x$framevp)))
}

heightDetails.frame <- function(x) {
  if (is.null(x$framevp))
    unit(1, "null")
  else
    sum(layout.heights(viewport.layout(x$framevp)))
}

frameDim <- function(frame) {
  if (is.null(frame$framevp))
    rep(0, 2)
  else
    c(layout.nrow(viewport.layout(frame$framevp)),
      layout.ncol(viewport.layout(frame$framevp)))
}

################
# cellGrob class
################
cellViewport <- function(col, row, border) {
  vp <- viewport(layout.pos.col=col, layout.pos.row=row)
  if (!is.null(border))
    vp <- vpStack(vp,
                  viewport(x=border[2],
                           y=border[1],
                           width=unit(1, "npc") - sum(border[c(2,4)]),
                           height=unit(1, "npc") - sum(border[c(1,3)]),
                           just=c("left", "bottom")))
  vp
}

cellGrob <- function(col, row, border, grob, dynamic, vp) {
  gTree(col=col, row=row, border=border, dynamic=dynamic,
        children=gList(grob), vp=vp, cl="cellGrob")
}

# For dynamically packed grobs, need to be able to
# recalculate cell sizes
widthDetails.cellGrob <- function(x) {
  if (x$dynamic)
    unit(1, "grobwidth", gPath(x$children[[1]]$name))
  else
    unit(1, "grobwidth", x$children[[1]])
}

heightDetails.cellGrob <- function(x) {
  if (x$dynamic)
    unit(1, "grobheight", gPath(x$children[[1]]$name))
  else
    unit(1, "grobheight", x$children[[1]])
}

################
# grid.place
################
# Place an object into an already existing cell of a frame ...
# ... for a grob on the display list
grid.place <- function(gPath, grob,
                       row=1, col=1,
                       redraw=TRUE) {
  grid.set(gPath,
           placeGrob(grid.get(gPath), grob, row, col),
           redraw)
}
  
# ... for a grob description
placeGrob <- function(frame, grob,
                      row=NULL, col=NULL) {
  if (!inherits(frame, "frame"))
    stop("Invalid frame")
  if (!is.grob(grob))
    stop("Invalid grob")
  dim <- frameDim(frame)
  if (is.null(row))
    row <- c(1, dim[1])
  if (is.null(col))
    col <- c(1, dim[2])
  if (length(row) == 1)
    row <- rep(row, 2)
  if (length(col) == 1)
    col <- rep(col, 2)
  if (min(row) < 1 || max(row) > dim[1] ||
      min(col) < 1 || max(col) > dim[2])
    stop("Invalid row and/or col (no such cell in frame layout)")
  cgrob <- cellGrob(col, row, NULL, grob, FALSE,
                    cellViewport(col, row, NULL))
  addGrob(frame, cgrob)
}

################
# grid.pack
################
num.col.specs <- function(side, col, col.before, col.after) {
  4 - sum(is.null(side) || any(c("top", "bottom") %in% side),
          is.null(col), is.null(col.before), is.null(col.after))
}

# We are assuming that checking has been done so that only one
# of these specifications has been given
col.spec <- function(side, col, col.before, col.after, ncol) {
  if (!is.null(side)) {
    if (side == "left")
      col <- 1
    else if (side == "right")
      col <- ncol + 1
  }
  else if (!is.null(col.before))
    col <- col.before
  else if (!is.null(col.after))
    col <- col.after + 1
  col
}

# We are assuming that checking has been done so that only one
# of these specifications has been given
new.col <- function(side, col, col.before, col.after, ncol) {
  # Special case ncol==0 for first grob added to frame
  result <- TRUE
  if (!is.null(col)) {
    # It is an error to specify a range for col which is outside 1..ncol
    if (length(col) == 2) 
      if (col[1] < 1 || col[2] > ncol)
        stop("`col' can only be a range of existing columns")
      else
        result <- FALSE
    # It is also an error to specify a single col outside 1..ncol+1
    else
      if (col < 1 || col > ncol + 1)
        stop("Invalid column specification")
      else
        result <- col == ncol+1
  }
  result
}

num.row.specs <- function(side, row, row.before, row.after) {
  4 - sum(is.null(side) || any(c("left", "right") %in% side),
          is.null(row), is.null(row.before), is.null(row.after))
}

# We are assuming that checking has been done so that only one
# of these specifications has been given
row.spec <- function(side, row, row.before, row.after, nrow) {
  if (!is.null(side)) {
    if (side == "top")
      row <- 1
    else if (side == "bottom")
      row <- nrow + 1
  }
  else if (!is.null(row.before))
    row <- row.before
  else if (!is.null(row.after))
    row <- row.after + 1
  row
}

# We are assuming that checking has been done so that only one
# of these specifications has been given
new.row <- function(side, row, row.before, row.after, nrow) {
  # Special case nrow==0 for first grob added to frame
  result <- TRUE
  if (!is.null(row)) {
    # It is an error to specify a range for row which is outside 1..nrow
    if (length(row) == 2) 
      if (row[1] < 1 || row[2] > nrow)
        stop("`row' can only be a range of existing rows")
      else
        result <- FALSE
    # It is also an error to specify a single row outside 1..nrow+1
    else
      if (row < 1 || row > nrow + 1)
        stop("Invalid row specification")
      else
        result <- row == nrow+1
  }
  result
}

mod.dims <- function(dim, dims, index, new.index, nindex, force) {
  # If adding a new row/col, add the new width/height to the list
  if (new.index)
    if (index == 1)
      dims <- unit.c(dim, dims)
    else if (index == nindex)
      dims <- unit.c(dims, dim)
    else
      dims <- unit.c(dims[1:(index-1)], dim, dims[index:nindex])
  # Otherwise, if force=TRUE, we override previous width/heights for the
  # row/col, otherotherwise, the width/height of the existing row/col
  # is the maximum of the previous width/height and the new width/height
  else {
    if (!force)
      dim <- max(dim, dims[index])
    if (index==1)
      if (nindex == 1)
        dims <- dim
      else
        dims <- unit.c(dim, dims[2:nindex])
    else if (index==nindex)
      dims <- unit.c(dims[1:(nindex-1)], dim)
    else
      dims <- unit.c(dims[1:(index-1)], dim, dims[(index+1):nindex])
  }
  dims
}

updateCol <- function(col, added.col) {
  old.col <- col
  # If grob$col is a range ...
  if (length(old.col) == 2) {
    if (added.col <= old.col[2])
      col <- c(old.col[1], old.col[2] + 1)
  }
  else
    if (added.col <= old.col)
      col <- old.col + 1
  col
}

updateRow <- function(row, added.row) {
  old.row <- row
  # If grob$row is a range ...
  if (length(old.row) == 2) {
    if (added.row <= old.row[2])
      row <- c(old.row[1], old.row[2] + 1)
  }
  else
    if (added.row <= old.row)
      row <- old.row + 1
  row
}

# FIXME:  Allow specification of respect for new row/col
# Pack a child grob within a frame grob ... 
# (a special sort of editing just for frame grobs)
# ... for a grob on the display list
grid.pack <- function(gPath, grob, redraw=TRUE,
                      side=NULL,
                      row=NULL, row.before=NULL, row.after=NULL,
                      col=NULL, col.before=NULL, col.after=NULL,
                      width=NULL, height=NULL,
                      force.width=FALSE, force.height=FALSE,
                      border=NULL, dynamic=FALSE) {
  grid.set(gPath,
           packGrob(grid.get(gPath), grob, side,
                    row, row.before, row.after,
                    col, col.before, col.after,
                    width, height, force.width, force.height,
                    border),
           redraw)
}

packGrob <- function(frame, grob,
                     side=NULL,
                     row=NULL, row.before=NULL, row.after=NULL,
                     col=NULL, col.before=NULL, col.after=NULL,
                     width=NULL, height=NULL,
                     force.width=FALSE, force.height=FALSE,
                     border=NULL, dynamic=FALSE) {
  if (!inherits(frame, "frame"))
    stop("Invalid frame")
  if (!is.grob(grob))
    stop("Invalid grob")
  # col/row can be given as a range, but I only want to know
  # about the min and max
  if (!is.null(col) & length(col) > 1) {
    col <- range(col)
    col.range <- TRUE
  }
  else
    col.range <- FALSE
  if (!is.null(row) & length(row) > 1) {
    row <- range(row)
    row.range <- TRUE
  }
  else
    row.range <- FALSE
  
  frame.vp <- frame$framevp
  if (is.null(frame.vp))
    frame.vp <- viewport()
  lay <- viewport.layout(frame.vp)
  if (is.null(lay)) {
    ncol <- 0
    nrow <- 0
  } else {
    ncol <- layout.ncol(lay) 
    nrow <- layout.nrow(lay) 
  }
  
  # (i) Check that the specifications of the location of the grob
  # give a unique location
  ncs <- num.col.specs(side, col, col.before, col.after)
  # If user does not specify a col, assume it is all cols
  if (ncs == 0) {
    # Allow for fact that this might be first grob packed
    if (ncol > 0) {
      col <- c(1, ncol)
      col.range <- TRUE
    }
    else
      col <- 1
    ncs <- 1
  }
  if (ncs != 1) 
    stop("Cannot specify more than one of side=[\"left\", \"right\"], col, col.before, or col.after")
  nrs <- num.row.specs(side, row, row.before, row.after)
  # If user does not specify a row, assume it is all rows
  if (nrs == 0) {
    # Allow for fact that this might be first grob packed
    if (nrow > 0) {
      row <- c(1, nrow)
      row.range <- TRUE
    }
    else
      row <- 1
    nrs <- 1
  }
  if (nrs != 1)
    stop("Must specify exactly one of side=[\"top\", \"bottom\"], row, row.before, or row.after")

  # (ii) Determine that location and check that it is valid
  new.col <- new.col(side, col, col.before, col.after, ncol)
  col <- col.spec(side, col, col.before, col.after, ncol)
  new.row <- new.row(side, row, row.before, row.after, nrow)
  row <- row.spec(side, row, row.before, row.after, nrow)

  # Wrap the child in a "cellGrob" to maintain additional info
  # (like row and col occupied in frame)
  # Need to do this here so can create widths/heights based on this cell grob
  if (!is.null(grob))
    cgrob <- cellGrob(col, row, border, grob, dynamic,
                      cellViewport(col, row, border))
  
  # (iii) If width and height are not given, take them from the child
  #       NOTE:  if dynamic is TRUE then use a gPath to the child
  if (is.null(width))
    if (is.null(grob))
      width <- unit(1, "null")
    else
      if (dynamic)
        width <- unit(1, "grobwidth", gPath(cgrob$name))
      else
        width <- unit(1, "grobwidth", cgrob)
  if (is.null(height))
    if (is.null(grob))
      height <- unit(1, "null")
    else
      if (dynamic)
        height <- unit(1, "grobheight", gPath(cgrob$name))
      else
        height <- unit(1, "grobheight", cgrob)
  # If there is a border, include it in the width/height
  if (!is.null(border)) {
    width <- sum(border[2], width, border[4])
    height <- sum(border[1], height, border[3])
  }
  
  # (iv) Update the frame.vp of the frame (possibly add new row/col,
  # possibly update existing widths/heights and respect)
  if (new.col) ncol <- ncol + 1
  if (new.row) nrow <- nrow + 1
  # If we are creating the frame.vp$layout for the first time then
  # we have to initialise the layout widths and heights
  if (is.null(lay)) {
    widths <- width
    heights <- height
  } else {
    # DO NOT modify widths/heights if the grob is being added to
    # multiple columns/rows
    if (col.range)
      widths <- layout.widths(lay)
    else
      widths <- mod.dims(width, layout.widths(lay), col, new.col, ncol,
                         force.width)
    if (row.range)
      heights <- layout.heights(lay)
    else
      heights <- mod.dims(height, layout.heights(lay), row, new.row, nrow,
                          force.height)
  }
  frame.vp$layout <- grid.layout(ncol=ncol, nrow=nrow,
                                 widths=widths, height=heights)

  # Modify the locations (row, col) of existing children in the frame
  if (new.col || new.row) {
    for (i in childNames(frame)) {
      child <- getGrob(frame, i)
      if (new.col) {
        newcol <- updateCol(child$col, col)
        child <- editGrob(child, col=newcol,
                          vp=cellViewport(newcol, child$row, child$border))
      }
      if (new.row) {
        newrow <- updateRow(child$row, row)
        child <- editGrob(child, row=newrow,
                          vp=cellViewport(child$col, newrow, child$border))
      }
      frame <- addGrob(frame, child)
    }
  }

  # Add the new grob to the frame
  if (!is.null(grob)) {
    frame <- addGrob(frame, cgrob)
  }
  
  editGrob(frame, framevp=frame.vp)
}

