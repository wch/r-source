interaction.plot <-
    function(x.factor, trace.factor, response, fun=mean,
             type = c("l", "p"), legend = TRUE,
             trace.label=deparse(substitute(trace.factor)), fixed=FALSE,
             xlab = deparse(substitute(x.factor)), ylab = ylabel,
             ylim = range(cells, na.rm=TRUE),
             lty = nc:1, col = 1, pch = c(1:9, 0, letters), ...)
{
    ylabel <- paste(deparse(substitute(fun)), "of ",
                    deparse(substitute(response)))
    type <- match.arg(type)
    cells <- tapply(response, list(x.factor, trace.factor), fun)
    nr <- nrow(cells); nc <- ncol(cells)
    xvals <- 1:nr
    ## See if the x.factor labels are a sensible scale
    if(is.ordered(x.factor)) {
        wn <- getOption("warn")
        options(warn=-1)
        xnm <- as.numeric(levels(x.factor))
        options(warn=wn)
        if(!any(is.na(xnm))) xvals <- xnm
    }
    xlabs <- rownames(cells)
    ylabs <- colnames(cells)
    nch <- max(sapply(ylabs, nchar))
    if(is.null(xlabs)) xlabs <- as.character(xvals)
    if(is.null(ylabs)) ylabs <- as.character(1:nc)
    xlim <- range(xvals)
    xleg <- xlim[2] + 0.05 * diff(xlim)
    xlim <- if(legend) xlim + c(-0.2/nr, 0.2 + 0.02*nch) * diff(xlim)
    else xlim + c(-0.2/nr, 0.2/nr) * diff(xlim)
    matplot(xvals, cells, ..., type = type,  xlim = xlim, ylim = ylim,
            xlab = xlab, ylab = ylab, xaxt = "n",
            col = col, lty = lty, pch = pch)
    mtext(xlabs, 1, at = xvals)
    if(legend) {
        yrng <- diff(ylim)
        yleg <- ylim[2] - 0.1 * yrng
        text(xleg, ylim[2] - 0.05 * yrng, paste("  ", trace.label), adj = 0)
        if(!fixed) {
            ## sort them on the value at the last level of x.factor
            ord <- rev(order(cells[nr,  ]))
            ylabs <- ylabs[ord]
            lty <- lty[1 + (ord - 1) %% length(lty)]
            col <- col[1 + (ord - 1) %% length(col)]
            pch <- pch[ord]
        }
        if(type == "l")
            legend(xleg, yleg, bty = "n", legend = ylabs, lty = lty, col = col)
        else
            legend(xleg, yleg, bty = "n", legend = ylabs, col = col, pch = pch)
    }
    invisible()
}

