sunflowerplot <-
    function(x, y = NULL, number, log = "",
             xlab = NULL, ylab = NULL, xlim = NULL, ylim = NULL,
             add = FALSE, rotate = FALSE, cex = par("cex"), cex.fact =  1.6,
             pch = 16, seg.col = "yellow3", seg.lwd = 2.5, ...)
{
    ## Argument "checking" as plot.default:
    xlabel <- if (!missing(x)) deparse(substitute(x))
    ylabel <- if (!missing(y)) deparse(substitute(y))
    xy <- xy.coords(x, y, xlabel, ylabel, log)
    if(!add) {
        xlab <- if (is.null(xlab)) xy$xlab else xlab
        ylab <- if (is.null(ylab)) xy$ylab else ylab
        xlim <- if (is.null(xlim)) range(xy$x[is.finite(xy$x)]) else xlim
        ylim <- if (is.null(ylim)) range(xy$y[is.finite(xy$y)]) else ylim
    }
    n <- length(xy$x)
    if(missing(number)) {
        orderxy <- order(xy$x, xy$y)
        x <- xy$x[orderxy]
        y <- xy$y[orderxy]
        first <- c(TRUE, (x[-1] != x[-n]) | (y[-1] != y[-n]))
        x <- x[first]
        y <- y[first]
        number <- diff(c((1:n)[first], n + 1))
    } else {
        if(length(number) != n)
            stop("number must have same length as x & y !")
        np <- number > 0
        x <- xy$x[np]
        y <- xy$y[np]
        number <- number[np]
    }
    n <- length(x)
    if(!add)
        plot(x, y, xlab = xlab, ylab = ylab,
             xlim=xlim, ylim=ylim, log=log, type = "n", ...)

    n.is1 <- number == 1
    if(any(n.is1))
        points(x[ n.is1], y[ n.is1], pch = pch, cex = cex)
    if(any(!n.is1)) {
        points(x[!n.is1], y[!n.is1], pch = pch, cex = cex / cex.fact)
        i.multi <- (1:n)[number > 1]
        ppin <- par("pin")
        pusr <- par("usr")
        ## The next three lines are somewhat dubious....
        size <- cex * par("csi")
        xr <- size * abs(pusr[2] - pusr[1])/ppin[1]
        yr <- size * abs(pusr[4] - pusr[3])/ppin[2]

        i.rep <- rep(i.multi, number[number > 1])
        z <- numeric()
        for(i in i.multi)
            z <- c(z, 1:number[i] + if(rotate) runif(1) else 0)
        deg <- (2 * pi * z)/number[i.rep]
        segments(x[i.rep], y[i.rep],
                 x[i.rep] + xr * sin(deg),
                 y[i.rep] + yr * cos(deg),
                 col=seg.col, lwd = seg.lwd)
    }
    invisible(list(x=x, y=y, number=number))
}
