"persp" <-
function (x = seq(0, 1, len = nrow(z)), y = seq(0, 1, len = ncol(z)), 
    z, xlim = range(x), ylim = range(y), zlim = range(z, na.rm = TRUE), 
    theta = 0, phi = 15, r = sqrt(3), d = 1, scale = TRUE, expand = 1, 
    col, border, ltheta = -135, lphi = 0, shade = NA, box = TRUE, ...) 
{
    ## labcex is disregarded since we do NOT yet put  ANY labels...
    if (missing(z)) {
        if (!missing(x)) {
            if (is.list(x)) {
                z <- x$z
                y <- x$y
                x <- x$x
            }
            else {
                z <- x
                x <- seq(0, 1, len = nrow(z))
            }
        }
        else stop("no `z' matrix specified")
    }
    else if (is.list(x)) {
        y <- x$y
        x <- x$x
    }
    if (any(diff(x) <= 0) || any(diff(y) <= 0)) 
        stop("increasing x and y values expected")
    if (missing(col)) 
        col <- par("bg")
    if (missing(border)) 
        border <- par("fg")
    .Internal(persp(x, y, z, xlim, ylim, zlim, theta, phi, r, d,
              scale, expand, col, border, ltheta, lphi, shade,
              box, ...))
}
