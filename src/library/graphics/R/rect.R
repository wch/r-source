rect <-
  function (xleft, ybottom, xright, ytop, density = NULL, angle = 45,
            col = NULL, border = NULL, lty = NULL, lwd = par("lwd"), ...)
{
    if (is.numeric(density) && all(is.na(density) | density < 0))
        density <- NULL
    if (!is.null(density) && !is.null(angle)) {
        if (is.logical(border)) {
            if (border) border <- col
            else border <- NA
        }
        n <- range(length(xleft), length(xright),
                   length(ybottom), length(ytop))
        if (n[1] == 0)
            stop("invalid rectangle specification")
        n <- n[2]
        x <- rbind(rep.int(NA, n), xleft, xright, xright, xleft)[-1]
        y <- rbind(rep.int(NA, n), ybottom, ybottom, ytop, ytop)[-1]
        polygon(x, y, col = col, border = border, lty = lty, lwd = lwd,
                density = density, angle = angle, ...)
    }
    else
        .Internal(rect(as.double(xleft), as.double(ybottom),
                       as.double(xright), as.double(ytop),
                       col = col, border = border,
                       lty = lty, lwd = lwd, ...))
}
