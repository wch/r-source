######################################
# Stuff for lpack()
######################################

width.details.frame <- function(x) {
  sum(layout.widths(viewport.layout(x$frame.vp)))
}

height.details.frame <- function(x) {
  sum(layout.heights(viewport.layout(x$frame.vp)))
}

draw.frame.child <- function(grob) {
  temp.vp <- viewport(layout.pos.col=grob$col,
                      layout.pos.row=grob$row)
  push.viewport(temp.vp, recording=FALSE)
  if (!is.null(grob$border))
    push.viewport(viewport(x=grob$border[2],
                           y=grob$border[1],
                           width=unit(1, "npc") - sum(grob$border[c(2,4)]),
                           height=unit(1, "npc") - sum(grob$border[c(1,3)]),
                           just=c("left", "bottom")),
                  recording=FALSE)
  grid.draw(grob, recording=FALSE)
  if (!is.null(grob$border))
    pop.viewport(recording=FALSE)
  pop.viewport(recording=FALSE)
}

draw.details.frame <- function(x, x.wrapped, recording=TRUE) {
  if (!is.null(x$frame.vp))
    push.viewport(x$frame.vp, recording=FALSE)
  lapply(x$children, draw.frame.child)
  if (!is.null(x$frame.vp))
    pop.viewport(recording=FALSE)
}

# NOTE that this never produces any actual graphical output
# (there is nothing to draw) BUT it is important to use
# draw=TRUE if you want to pack the frame interactively.
# This ensures that the frame is on the .grid.display.list
# so that the editing that occurs in grid.pack() will redraw the
# frame when it forces a draw.all()
grid.frame <- function(layout=NULL, vp=NULL, gp=gpar(), draw=FALSE) {
  if (!is.null(layout))
    frame.vp <- viewport(layout=layout)
  else
    frame.vp <- NULL
  grid.grob(list(children=NULL, vp=vp, gp=gp, frame.vp=frame.vp),
        c("frame", "collection"), draw=draw)
}

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

updateCol <- function(grob, added.col) {
  old.col <- grob$col
  # If grob$col is a range ...
  if (length(old.col) == 2) {
    if (added.col <= old.col[2])
      grob$col <- c(old.col[1], old.col[2] + 1)
  }
  else
    if (added.col <= old.col)
      grob$col <- old.col + 1
  grob
}

updateRow <- function(grob, added.row) {
  old.row <- grob$row
  # If grob$row is a range ...
  if (length(old.row) == 2) {
    if (added.row <= old.row[2])
      grob$row <- c(old.row[1], old.row[2] + 1)
  }
  else
    if (added.row <= old.row)
      grob$row <- old.row + 1
  grob
}

# This guy is just a simpler interface to grid.pack(), with
# the focus more on just "placing" a grob within the existing
# layout of a frame, without modifying that layout in any way
# In this way, it is basically just a more convenient way of
# locating grobs within a viewport with a layout
# NOTE that it relies on intimate knowledge of grid.pack
# to make the minimum impact on the existing layout
# THEREFORE it is fragile if grid.pack changes
# In particular, it makes sure that the widths/heights of
# the layout are untouched by specifying the row and col as
# a range
grid.place <- function(frame, grob, grob.name="", draw=TRUE,
                       row=1, col=1) {
  if (length(row) == 1)
    row <- rep(row, 2)
  if (length(col) == 1)
    col <- rep(col, 2)
  grid.pack(frame, grob, grob.name, draw,
            col=col, row=row,
            # Just dummy values;  they will be ignored by grid.pack
            width=unit(1, "null"), height=unit(1, "null"))
}

# Pack a child grob within a frame grob
# (a special sort of editing just for frame grobs)
# FIXME:  Allow specification of respect for new row/col
grid.pack <- function(frame, grob, grob.name="", draw=TRUE,
                      side=NULL,
                      row=NULL, row.before=NULL, row.after=NULL,
                      col=NULL, col.before=NULL, col.after=NULL,
                      width=NULL, height=NULL,
                      force.width=FALSE, force.height=FALSE,
                      border=NULL) {
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
  
  frame.vp <- grid.get(frame, "frame.vp")
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
  
  # (iii) If width and height are not given, take them from the child
  if (is.null(width))
    if (is.null(grob))
      width <- unit(1, "null")
    else
      width <- unit(1, "grobwidth", grob)
  if (is.null(height))
    if (is.null(grob))
      height <- unit(1, "null")
    else
      height <- unit(1, "grobheight", grob)
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
  # NOT SURE WHAT THIS WAS DOING HERE
  # respect <- layout.respect(lay)
  frame.vp$layout <- grid.layout(ncol=ncol, nrow=nrow,
                                 widths=widths, height=heights)
  children <- grid.get(frame, "children")
  # Modify the locations (row, col) of existing children in the frame
  if (new.col)
    children <- lapply(children, updateCol, col)
  if (new.row)
    children <- lapply(children, updateRow, row)
  if (!is.null(grob)) {
    # Give the new grob a record of its location (row, col) in the frame
    grob$row <- row
    grob$col <- col
    grob$border <- border
    children <- c(children, list(grob))
  }
  grid.edit(frame, grid.prop.list(children=children, frame.vp=frame.vp), redraw=draw)
}

