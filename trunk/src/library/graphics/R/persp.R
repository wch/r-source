persp <- function(x, ...) UseMethod("persp")

persp.default <-
function (x = seq(0, 1, len = nrow(z)), y = seq(0, 1, len = ncol(z)),
    z, xlim = range(x), ylim = range(y), zlim = range(z, na.rm = TRUE),
    xlab = NULL, ylab = NULL, zlab = NULL, main = NULL, sub = NULL,
    theta = 0, phi = 15, r = sqrt(3), d = 1, scale = TRUE, expand = 1,
    col = "white", border = NULL, ltheta = -135, lphi = 0, shade = NA,
    box = TRUE, axes = TRUE, nticks = 5, ticktype = "simple", ...)
{
    if (is.null(xlab))
        xlab <- if (!missing(x)) deparse(substitute(x)) else "X"
    if (is.null(ylab))
        ylab <- if (!missing(y)) deparse(substitute(y)) else "Y"
    if (is.null(zlab))
        zlab <- if (!missing(z)) deparse(substitute(z)) else "Z"
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
        else stop("no 'z' matrix specified")
    }
    else if (is.list(x)) {
        y <- x$y
        x <- x$x
    }
    if (any(diff(x) <= 0) || any(diff(y) <= 0))
        stop("increasing 'x' and 'y' values expected")
    ticktype <- pmatch(ticktype, c("simple", "detailed"))
    r <- .Internal(persp(x, y, z, xlim, ylim, zlim, theta, phi, r, d,
                         scale, expand, col, border, ltheta, lphi, shade,
                         box, axes, nticks, ticktype,
                         as.character(xlab), as.character(ylab),
                         as.character(zlab), ...))
    for(fun in getHook("persp")) {
        if(is.character(fun)) fun <- get(fun)
        try(fun())
    }
    if(!is.null(main) || !is.null(sub))
        title(main = main, sub = sub, ...)
    invisible(r)
}
