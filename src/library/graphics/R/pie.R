pie <-
    function (x, labels = names(x), edges = 200, radius = 0.8,
              clockwise = FALSE, init.angle = if(clockwise) 90 else 0,
              density = NULL, angle = 45, col = NULL, border = NULL, lty = NULL,
              main = NULL, ...)
{
    if (!is.numeric(x) || any(is.na(x) | x <= 0))
	stop("'x' values must be positive.")
    if (is.null(labels))
	labels <- as.character(1:length(x))
    x <- c(0, cumsum(x)/sum(x))
    dx <- diff(x)
    nx <- length(dx)
    plot.new()
    # NOTE: this needs to happen AFTER the plot.new so that
    # we enquire about the CURRENT plot region size, not the
    # PREVIOUS plot region size
    pin <- par("pin")
    xlim <- ylim <- c(-1, 1)
    if (pin[1] > pin[2]) xlim <- (pin[1]/pin[2]) * xlim
    else ylim <- (pin[2]/pin[1]) * ylim
    plot.window(xlim, ylim, "", asp = 1)
    if (is.null(col))
        col <- if(is.null(density))
            c("white", "lightblue", "mistyrose", "lightcyan", "lavender", "cornsilk")
        else par("fg")
    col <- rep(col, length.out = nx)
    border <- rep(border, length.out = nx)
    lty <- rep(lty, length.out = nx)
    angle <- rep(angle, length.out = nx)
    density <- rep(density, length.out = nx)
    twopi <- if(clockwise) -2*pi else 2*pi
    t2xy <- function(t) {
        t2p <- twopi*t + init.angle * pi/180
        list(x = radius * cos(t2p), y = radius * sin(t2p))
    }
    for (i in 1:nx) {
	n <- max(2, floor(edges * dx[i]))
	P <- t2xy(seq(x[i], x[i + 1], length = n))
	polygon(c(P$x, 0), c(P$y, 0), density = density[i], angle = angle[i],
                border = border[i], col = col[i], lty = lty[i])
	P <- t2xy(mean(x[i + 0:1]))
        if(!is.na(lab <- labels[i]) && lab != "") {
            lines(c(1, 1.05)*P$x, c(1, 1.05)*P$y)
            text(1.1*P$x, 1.1*P$y, lab, xpd = TRUE,
                 adj = ifelse(P$x < 0, 1, 0), ...)
        }
    }
    title(main = main, ...)
    invisible(NULL)
}
