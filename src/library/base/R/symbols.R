symbols <-
  function (x, y, circles, squares, rectangles, stars,
            thermometers, boxplots, inches = TRUE, add = FALSE,
            fg = 1, bg = NA, xlab = "", ylab ="", ...)
{
  count <- 0
  if (!missing(circles)) {
    count <- count + 1
    data <- circles
    type <- 1
  }
  if (!missing(squares)) {
    count <- count + 1
    data <- squares
    type <- 2
  }
  if (!missing(rectangles)) {
    count <- count + 1
    data <- rectangles
    type <- 3
  }
  if (!missing(stars)) {
    count <- count + 1
    data <- stars
    type <- 4
  }
  if (!missing(thermometers)) {
    count <- count + 1
    data <- thermometers
    type <- 5
  }
  if (!missing(boxplots)) {
    count <- count + 1
    data <- boxplots
    type <- 6
  }
  if (count != 1)
    stop("exactly one symbol type can be specified in \"symbols\"");
  if (!add) {
    # Expand the range by 0.2 - wild guess
    xlim <- range(x, na.rm = TRUE)
    ylim <- range(y, na.rm = TRUE)
    xlim <- xlim + c(-1, 1) * (0.1 * diff(xlim))
    ylim <- ylim + c(-1, 1) * (0.1 * diff(ylim))
    plot(NA, NA, type="n", ylim=ylim, xlim = xlim, xlab=xlab, ylab=ylab, ...)
  }
  .Internal(symbols(x, y, type, data, inches, bg, fg, ...))
}
