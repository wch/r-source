boxplot <- function(x, ...) UseMethod("boxplot")

boxplot.default <-
function(x, ..., range = 1.5, width = NULL, varwidth = FALSE,
         notch = FALSE, names, boxwex = 0.8,
	 data = parent.frame(), plot = TRUE,
         border = par("fg"), col = NULL, log = "", pars = NULL,
         horizontal = FALSE, add = FALSE)
{
    args <- list(x, ...)
    namedargs <-
	if(!is.null(attributes(args)$names))
	    attributes(args)$names != ""
	else
	    rep(FALSE, length = length(args))
    pars <- c(args[namedargs], pars)
    groups <-
	if(is.language(x)) {
            warning("Using `formula' in boxplot.default -- shouldn't boxplot.formula be called?")
	    if(inherits(x, "formula") && length(x) == 3) {
		groups <- eval(x[[3]], data, parent.frame())
		x <- eval(x[[2]], data, parent.frame())
		split(x, groups)
	    }
	}
	else {
	    groups <- args[!namedargs]
	    if(length(groups) == 1 && is.list(x)) x else groups
	}
    if(0 == (n <- length(groups)))
	stop("invalid first argument")
    if(length(class(groups)))
	groups <- unclass(groups)
    if(!missing(names))
	attr(groups, "names") <- names
    else {
	if(is.null(attr(groups, "names")))
	    attr(groups, "names") <- 1:n
        names <- attr(groups, "names")
    }
    for(i in 1:n)
	groups[i] <- list(boxplot.stats(groups[[i]], range)) # do.conf=notch)
    stats <- matrix(0,nr=5,nc=n)
    conf  <- matrix(0,nr=2,nc=n)
    ng <- out <- group <- numeric(0)
    ct <- 1
    for(i in groups) {
	stats[,ct] <- i$stats
        conf [,ct] <- i$conf
        ng <- c(ng, i$n)
        if((lo <- length(i$out))) {
            out   <- c(out,i$out)
            group <- c(group, rep(ct, lo))
        }
        ct <- ct+1
    }
    z <- list(stats = stats, n = ng, conf = conf, out = out, group = group,
              names = names)
    if(plot) {
	bxp(z, width, varwidth = varwidth, notch = notch, boxwex = boxwex,
            border = border, col = col, log = log, pars = pars,
            horizontal = horizontal, add = add)
	invisible(z)
    }
    else z
}

boxplot.formula <- function(formula, data = NULL, subset, na.action, ...)
{
    if(missing(formula) || (length(formula) != 3))
        stop("formula missing or incorrect")
    if(missing(na.action))
        na.action <- getOption("na.action")
    m <- match.call(expand.dots = FALSE)
    if(is.matrix(eval(m$data, parent.frame())))
        m$data <- as.data.frame(data)
    m$... <- NULL
    m[[1]] <- as.name("model.frame")
    mf <- eval(m, parent.frame())
    response <- attr(attr(mf, "terms"), "response")
    boxplot(split(mf[[response]], mf[[-response]]), ...)
}

boxplot.stats <- function(x, coef = 1.5, do.conf=TRUE, do.out=TRUE)
{
    nna <- !is.na(x)
    n <- length(nna)                    # including +/- Inf
    stats <- fivenum(x, na.rm = TRUE)
    iqr <- diff(stats[c(2, 4)])
    out <- x < (stats[2] - coef * iqr) | x > (stats[4] + coef * iqr)
    if(coef > 0) stats[c(1, 5)] <- range(x[!out], na.rm = TRUE)
    conf <- if(do.conf)
        stats[3] + c(-1.58, 1.58) * diff(stats[c(2, 4)]) / sqrt(n)
    list(stats = stats, n = n, conf = conf,
         out = if(do.out) x[out & nna] else numeric(0))
}

bxp <- function(z, notch=FALSE, width=NULL, varwidth=FALSE,
	        notch.frac = 0.5, boxwex = 0.8,
		border=par("fg"), col=NULL, log="", pars=NULL,
                frame.plot = axes,
                horizontal = FALSE, add = FALSE, ...)
{
    pars <- c(pars, list(...))

    bplt <- function(x, wid, stats, out, conf, notch, border, col, horizontal)
    {
	## Draw single box plot
	if(!any(is.na(stats))) {
	    ## stats = +/- Inf:	 polygon & segments should handle
	    wid <- wid/2
            if (horizontal) {

                if (notch) {
                    xx <- x + wid * c(-1, 1, 1, notch.frac, 1, 1,
                                      -1, -1, -notch.frac, -1)
                    yy <- c(stats[c(2, 2)], conf[1], stats[3], conf[2],
                            stats[c(4, 4)], conf[2], stats[3], conf[1])
                    polygon(yy, xx, col = col, border = border)
                    segments(stats[3], x - wid/2, stats[3], x + wid/2,
                             col = border)
                }
                else {
                    xx <- x + wid * c(-1, 1, 1, -1)
                    yy <- stats[c(2, 2, 4, 4)]
                    polygon(yy, xx, col = col, border = border)
                    segments(stats[3], x - wid, stats[3], x + wid,
                             col = border)
                }
                segments(stats[c(1, 5)], rep(x, 2), stats[c(2, 4)], rep(x, 2),
                         lty = "dashed", col = border)
                segments(stats[c(1, 5)], rep(x - wid/2, 2), stats[c(1, 5)],
                         rep(x + wid/2, 2), col = border)
                points(out, rep(x, length(out)), col = border)

            }
            else { ## vertical

                if(notch) {
                    xx <- x+wid*c(-1,1, 1, notch.frac, 1,
                                  1,-1,-1,-notch.frac,-1)
                    yy <- c(stats[c(2,2)],conf[1],stats[3],conf[2],
                            stats[c(4,4)],conf[2],stats[3],conf[1])
                    polygon(xx, yy, col=col, border=border)
                    segments(x-wid/2,stats[3], x+wid/2,stats[3], col=border)
                }
                else {
                    xx <- x+wid*c(-1,1,1,-1)
                    yy <- stats[c(2,2,4,4)]
                    polygon(xx, yy, col=col, border=border)
                    segments(x-wid,stats[3],x+wid,stats[3],col=border)
                }
                segments(rep(x,2),stats[c(1,5)], rep(x,2),
                         stats[c(2,4)], lty="dashed",col=border)
                segments(rep(x-wid/2,2),stats[c(1,5)],rep(x+wid/2,2),
                         stats[c(1,5)],col=border)
                points(rep(x,length(out)), out, col=border)
            }
	    if(any(inf <- !is.finite(out))) {
		## FIXME: should MARK on plot !! (S-plus doesn't either)
		warning(paste("Outlier (",
			      paste(unique(out[inf]),collapse=", "),
			      ") in ", paste(x,c("st","nd","rd","th")
					     [pmin(4,x)], sep=""),
			      " boxplot are NOT drawn", sep=""))
	    }
	}
    }## bplt

    if(!is.list(z) || 0 == (n <- length(z$n)))
	stop("invalid first argument")
    ## just for compatibility with S
    if(is.null(z$out))	 z$out	 <- vector(length=0)
    if(is.null(z$group)) z$group <- vector(length=0)
    if(is.null(pars$ylim))
	ylim <- range(z$stats[is.finite(z$stats)],
		      z$out  [is.finite(z$out)],
		      if(notch)
		      z$conf [is.finite(z$conf)])
    else {
	ylim <- pars$ylim
	pars$ylim <- NULL
    }
    width <-
	if(!is.null(width)) {
	    if(length(width) != n | any(is.na(width)) | any(width <= 0))
		stop("invalid boxplot widths")
	    boxwex * width/max(width)
	}
	else if(varwidth) boxwex * sqrt(z$n/max(z$n))
	else if(n == 1) 0.5 * boxwex
	else rep(boxwex, n)

    if(missing(border) || length(border)==0)
	border <- par("fg")

    if (!add) {
    	plot.new()
    	## shall we switch log for horizontal with
        ## switch(log, x="y", y="x", log) ??
    	if (horizontal)
            plot.window(ylim = c(0.5, n + 0.5), xlim = ylim, log = log)
        else
            plot.window(xlim = c(0.5, n + 0.5), ylim = ylim, log = log)
    }
    for(i in 1:n)
	bplt(i, wid=width[i],
	     stats= z$stats[,i],
	     out  = z$out[z$group==i],
	     conf = z$conf[,i],
	     notch= notch,
	     border=border[(i-1)%%length(border)+1],
	     col = if(is.null(col)) col else col[(i-1)%%length(col)+1],
             horizontal=horizontal)

    axes <- is.null(pars$axes)
    if(!axes) { axes <- pars$axes; pars$axes <- NULL }
    if(axes) {
        if (n > 1)
            do.call("axis", c(list(side = 1 + horizontal, at = 1:n, labels
                 = z$names), pars[names(pars) %in% c("xaxt", "yaxt", "las")]))
        do.call("axis", c(list(side = 2 - horizontal),
                          pars[names(pars) %in% c("xaxt", "yaxt", "las")]))
    }
    do.call("title", pars)
    if(frame.plot)
        box()
    invisible(1:n)
}
