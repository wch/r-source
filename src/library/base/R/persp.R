"persp" <-
function (x = seq(0, 1, len = nrow(z)), y = seq(0, 1, len = ncol(z)), 
    z, xlim = range(x), ylim = range(y), zlim = range(z, na.rm = TRUE), 
    xlab = NULL, ylab = NULL, zlab = NULL,
    theta = 0, phi = 15, r = sqrt(3), d = 1, scale = TRUE, expand = 1, 
    col = NULL, border = NULL, ltheta = -135, lphi = 0, shade = NA,
    box = TRUE, axes = TRUE, nticks = 5, ticktype = "simple", ...) 
{
    if (is.null(xlab)) 
        xlabel <- if (!missing(x)) deparse(substitute(x)) else "X"
    else
	xlabel <- xlab
    if (is.null(ylab)) 
        ylabel <- if (!missing(y)) deparse(substitute(y)) else "Y"
    else
	ylabel <- ylab
    if (is.null(zlab)) 
        zlabel <- if (!missing(z)) deparse(substitute(z)) else "Z"
    else
	zlabel <- zlab
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
    ticktype <- pmatch(ticktype, c("simple", "detailed"))
    .Internal(persp(x, y, z, xlim, ylim, zlim, theta, phi, r, d,
              scale, expand, col, border, ltheta, lphi, shade,
              box, axes, nticks, ticktype, xlabel, ylabel, zlabel, ...))
}
