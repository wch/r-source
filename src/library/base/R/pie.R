pie <-
    function (x, labels = names(x), edges = 200, radius = 0.8,
              density = NULL, angle = 45, col = NULL,
              main = NULL, ...)
{
    if (!is.numeric(x) || any(is.na(x) | x <= 0))
	stop("pie: `x' values must be positive.")
    if (is.null(labels))
	labels <- as.character(1:length(x))
    x <- c(0, cumsum(x)/sum(x))
    dx <- diff(x)
    pin <- par("pin")
    xlim <- ylim <- c(-1, 1)
    if (pin[1] > pin[2]) xlim <- (pin[1]/pin[2]) * xlim
    else ylim <- (pin[2]/pin[1]) * ylim
    plot.new()
    plot.window(xlim, ylim, "", asp = 1)
    nx <- length(dx)
    if (is.null(col))
        col <- if(is.null(density))
            c("white", "lightblue", "mistyrose", "lightcyan", "lavender", "cornsilk")
        else par("fg")
    col <- rep(col, length = nx)
    angle <- rep(angle, length = nx)
    density <- rep(density, length = nx)
    for (i in 1:nx) {
	n <- max(2, floor(edges * dx[i]))
	t2p <- 2*pi * seq(x[i], x[i + 1], length = n)
	xc <- c(cos(t2p), 0) * radius
	yc <- c(sin(t2p), 0) * radius
	polygon(xc, yc, col = col[i],
                angle = angle[i], density = density[i])
	t2p <- 2*pi * mean(x[i + 0:1])
	xc <- cos(t2p) * radius
	yc <- sin(t2p) * radius
	lines(c(1, 1.05)*xc, c(1, 1.05)*yc)
	text(1.1*xc, 1.1*yc, labels[i],
	     xpd = TRUE, adj = ifelse(xc < 0, 1, 0))
    }
    title(main = main, ...)
    invisible(NULL)
}
