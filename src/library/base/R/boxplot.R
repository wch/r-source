boxplot <- function(x, ...) UseMethod("boxplot")

boxplot.default <-
function(x, ..., range = 1.5, width = NULL, varwidth = FALSE,
         notch = FALSE, outline = TRUE, names, boxwex = 0.8, plot = TRUE,
         border = par("fg"), col = NULL, log = "", pars = NULL,
         horizontal = FALSE, add = FALSE, at = NULL)
{
    args <- list(x, ...)
    namedargs <-
	if(!is.null(attributes(args)$names))
	    attributes(args)$names != ""
	else
	    rep(FALSE, length = length(args))
    pars <- c(args[namedargs], pars)
    groups <- if(is.list(x)) x else args[!namedargs]
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
            outline = outline, horizontal = horizontal, add = add, at = at)
	invisible(z)
    }
    else z
}

boxplot.formula <- function(formula, data = NULL, ..., subset)
{
    if(missing(formula) || (length(formula) != 3))
        stop("formula missing or incorrect")
    m <- match.call(expand.dots = FALSE)
    if(is.matrix(eval(m$data, parent.frame())))
        m$data <- as.data.frame(data)
    m$... <- NULL
    m[[1]] <- as.name("model.frame")
    mf <- eval(m, parent.frame())
    response <- attr(attr(mf, "terms"), "response")
    boxplot(split(mf[[response]], mf[-response]), ...)
}

boxplot.stats <- function(x, coef = 1.5, do.conf=TRUE, do.out=TRUE)
{
    nna <- !is.na(x)
    n <- sum(nna) # including +/- Inf
    stats <- fivenum(x, na.rm = TRUE)
    iqr <- diff(stats[c(2, 4)])
    if(coef < 0) stop("`coef' must not be negative")
    if(coef == 0)
	do.out <- FALSE
    else { # coef > 0
	out <- x < (stats[2] - coef * iqr) | x > (stats[4] + coef * iqr)
	if(any(out[nna])) stats[c(1, 5)] <- range(x[!out], na.rm = TRUE)
    }
    conf <- if(do.conf)
	stats[3] + c(-1.58, 1.58) * diff(stats[c(2, 4)]) / sqrt(n)
    list(stats = stats, n = n, conf = conf,
	 out = if(do.out) x[out & nna] else numeric(0))
}

bxp <- function(z, notch=FALSE, width=NULL, varwidth=FALSE,
	        outline = TRUE, notch.frac = 0.5, boxwex = 0.8,
		border=par("fg"), col=NULL, log="", pars=NULL,
                frame.plot = axes,
                horizontal = FALSE, add = FALSE, at = NULL, show.names=NULL,
                ...)
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
                do.call("points",c(list(out, rep(x, length(out))), pt.pars))
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
                do.call("points",c(list(rep(x,length(out)), out), pt.pars))
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
    if(is.null(at))
        at <- 1:n
    else if(length(at) != n)
        stop(paste("`at' must have same length as `z $ n', i.e.",n))
    ## just for compatibility with S
    if(is.null(z$out))
        z$out <- numeric()
    if(is.null(z$group) || !outline)
        z$group <- integer()
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
    pt.pars <- c(pars[names(pars) %in% c("pch", "cex", "bg")], col = border)

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
	bplt(at[i], wid=width[i],
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
        ax.pars <- pars[names(pars) %in% c("xaxt", "yaxt", "las")]
        if (is.null(show.names)) show.names <- n > 1
        if (show.names)
            do.call("axis", c(list(side = 1 + horizontal,
                                   at = at, labels = z$names), ax.pars))
        do.call("axis", c(list(side = 2 - horizontal), ax.pars))
    }
    do.call("title", pars)
    if(frame.plot)
        box()
    invisible(at)
}
