boxplot <- function(x, ...) UseMethod("boxplot")

boxplot.default <-
function(x, ..., range = 1.5, width = NULL, varwidth = FALSE,
         notch = FALSE, outline = TRUE, names, plot = TRUE,
         border = par("fg"), col = NULL, log = "",
         pars = list(boxwex = 0.8, staplewex = 0.5, outwex = 0.5),
         horizontal = FALSE, add = FALSE, at = NULL)
{
    args <- list(x, ...)
    namedargs <-
	if(!is.null(attributes(args)$names))
	    attributes(args)$names != ""
	else
	    rep(FALSE, length.out = length(args))
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
            group <- c(group, rep.int(ct, lo))
        }
        ct <- ct+1
    }
    z <- list(stats = stats, n = ng, conf = conf, out = out, group = group,
              names = names)
    if(plot) {
	bxp(z, width, varwidth = varwidth, notch = notch, log = log,
            border = border, col = col, pars = pars,
            outline = outline, horizontal = horizontal, add = add, at = at)
	invisible(z)
    }
    else z
}

boxplot.formula <-
    function(formula, data = NULL, ..., subset, na.action = NULL)
{
    if(missing(formula) || (length(formula) != 3))
        stop("formula missing or incorrect")
    m <- match.call(expand.dots = FALSE)
    if(is.matrix(eval(m$data, parent.frame())))
        m$data <- as.data.frame(data)
    m$... <- NULL
    m$na.action <- na.action # force use of default for this method
    m[[1]] <- as.name("model.frame")
    mf <- eval(m, parent.frame())
    response <- attr(attr(mf, "terms"), "response")
    boxplot(split(mf[[response]], mf[-response]), ...)
}

boxplot.stats <- function(x, coef = 1.5, do.conf=TRUE, do.out=TRUE)
{
    nna <- !is.na(x)
    n <- sum(nna)                       # including +/- Inf
    stats <- stats::fivenum(x, na.rm = TRUE)
    iqr <- diff(stats[c(2, 4)])
    if(coef < 0) stop(paste(sQuote("coef"), "must not be negative"))
    if(coef == 0)
	do.out <- FALSE
    else {                              # coef > 0
	out <- x < (stats[2] - coef * iqr) | x > (stats[4] + coef * iqr)
	if(any(out[nna])) stats[c(1, 5)] <- range(x[!out], na.rm = TRUE)
    }
    conf <- if(do.conf) stats[3] + c(-1.58, 1.58) * iqr / sqrt(n)
    list(stats = stats, n = n, conf = conf,
	 out = if(do.out) x[out & nna] else numeric(0))
}

bxp <- function(z, notch=FALSE, width=NULL, varwidth=FALSE, outline = TRUE,
		notch.frac = 0.5, log = "", border=par("fg"), col=par("bg"),
                pars = NULL, frame.plot = axes, horizontal = FALSE,
                add = FALSE, at = NULL, show.names = NULL, ...)
{
    pars <- c(pars, list(...))

    bplt <- function(x, wid, stats, out, conf, notch, xlog, i)
    {
	## Draw single box plot

	if(!any(is.na(stats))) {
	    ## stats = +/- Inf:	polygon & segments should handle

	    ## Compute 'x + w' -- "correctly" in log-coord. case:
	    xP <-
		if(xlog) function(x,w) x * exp(w)
		else function(x,w) x + w
	    wid <- wid/2
	    if (notch) {
		xx <- xP(x, wid * c(-1, 1, 1, notch.frac, 1,
				    1, -1,-1,-notch.frac,-1))
		yy <- c(stats[c(2, 2)], conf[1], stats[3], conf[2],
			stats[c(4, 4)], conf[2], stats[3], conf[1])
	    }
	    else {
		xx <- xP(x, wid * c(-1, 1, 1, -1))
		yy <- stats[c(2, 2, 4, 4)]
	    }
	    if(!notch) notch.frac <- 1
	    wntch <- notch.frac*wid

            ## the box filling over which to draw the rest:
            xypolygon(xx, yy, lty = "blank", col = boxfill[i])
            ## Median
	    xysegments(xP(x, -wntch), stats[3],
		       xP(x, +wntch), stats[3],
		       lty = medlty[i], lwd = medlwd[i], col = medcol[i])
	    xypoints(x, stats[3],
		     pch = medpch[i], cex = medcex[i], col= medcol[i], bg = medbg[i])
            ## Whiskers
	    xysegments(rep.int(x, 2), stats[c(1,5)],
		       rep.int(x, 2), stats[c(2,4)],
		       lty = whisklty[i], lwd = whisklwd[i], col = whiskcol[i])
	    xysegments(rep.int(xP(x, -wid * staplewex), 2), stats[c(1,5)],
		       rep.int(xP(x, +wid * staplewex), 2), stats[c(1,5)],
		       lty= staplelty[i], lwd= staplelwd[i], col= staplecol[i])
            ## finally the box borders
	    xypolygon(xx, yy, lty= boxlty[i], lwd= boxlwd[i], border= boxcol[i])

	    if ((nout <- length(out)) > 0) { ## Outliers
		xysegments(rep(x - wid * outwex, nout), out,
			   rep(x + wid * outwex, nout), out,
			   lty = outlty[i], lwd = outlwd[i], col = outcol[i])
		xypoints(rep.int(x, nout), out, pch = outpch[i],
			 cex = outcex[i], col = outcol[i], bg = outbg[i])
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
    } ## bplt

    if(!is.list(z) || 0 == (n <- length(z$n)))
	stop("invalid first argument")
    if(is.null(at))
	at <- 1:n
    else if(length(at) != n)
	stop(paste(sQuote("at"), " must have same length as ",
		   sQuote("z $ n"), ", i.e. ", n,
		   sep = ""))
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

    if(length(border) == 0) border <- par("fg")

    if (!add) {
	plot.new()
	## shall we switch log for horizontal with
	## switch(log, x="y", y="x", log) ??
	if (horizontal)
	    plot.window(ylim = c(0.5, n + 0.5), xlim = ylim, log = log)
	else
	    plot.window(xlim = c(0.5, n + 0.5), ylim = ylim, log = log)
    }
    xlog <- (par("ylog") && horizontal) || (par("xlog") && !horizontal)

    pcycle <- function(p, def1, def2=NULL)# or rather NA {to be rep()ed}?
	rep(if(length(p)) p else if(length(def1)) def1 else def2,
            length.out = n)
    boxlty    <- pcycle(pars$boxlty,	pars$lty, par("lty"))
    boxlwd    <- pcycle(pars$boxlwd,	pars$lwd, par("lwd"))
    boxcol    <- pcycle(pars$boxcol,	border)
    boxfill   <- pcycle(pars$boxfill,	col)
    boxwex    <- pcycle(pars$boxwex,	0.8 * {
        if(n <= 1) 1 else
        quantile(diff(sort(if(xlog) log(at) else at)), 0.10) })
    medlty    <- pcycle(pars$medlty,	pars$lty, par("lty"))
    medlwd    <- pcycle(pars$medlwd,	3*pars$lwd, 3*par("lwd"))
    medpch    <- pcycle(pars$medpch,	as.integer(NA))# NA when that works
    medcex    <- pcycle(pars$medcex,	pars$cex, par("cex"))
    medcol    <- pcycle(pars$medcol,	border)
    medbg     <- pcycle(pars$medbg,	pars$bg,  par("bg"))
    whisklty  <- pcycle(pars$whisklty,	pars$lty, "dashed")
    whisklwd  <- pcycle(pars$whisklwd,	pars$lwd, par("lwd"))
    whiskcol  <- pcycle(pars$whiskcol,	border)
    staplelty <- pcycle(pars$staplelty, pars$lty, par("lty"))
    staplelwd <- pcycle(pars$staplelwd, pars$lwd, par("lwd"))
    staplecol <- pcycle(pars$staplecol, border)
    staplewex <- pcycle(pars$staplewex,	0.5)
    outlty    <- pcycle(pars$outlty,    "blank")
    outlwd    <- pcycle(pars$outlwd,    pars$lwd, par("lwd"))
    outpch    <- pcycle(pars$outpch,    pars$pch, par("pch"))
    outcex    <- pcycle(pars$outcex,    pars$cex, par("cex"))
    outcol    <- pcycle(pars$outcol,    border)
    outbg     <- pcycle(pars$outbg,     pars$bg,  par("bg"))
    outwex    <- pcycle(pars$outwex,	0.5)

    width <-
	if(!is.null(width)) {
	    if(length(width) != n | any(is.na(width)) | any(width <= 0))
		stop("invalid boxplot widths")
	    boxwex * width/max(width)
	}
	else if(varwidth) boxwex * sqrt(z$n/max(z$n))
	else if(n == 1) 0.5 * boxwex
	else rep.int(boxwex, n)

    if(horizontal) {
        xypoints <- function(x, y, ...) points(y, x, ...)
        xypolygon <- function(x, y, ...) polygon(y, x, ...)
        xysegments <- function(x0, y0, x1, y1, ...) segments(y0, x0, y1, x1, ...)
    }
    else {
        xypoints <- points
        xypolygon <- polygon
        xysegments <- segments
    }

    for(i in 1:n)
	bplt(at[i], wid=width[i],
	     stats= z$stats[,i],
	     out  = z$out[z$group==i],
	     conf = z$conf[,i],
	     notch= notch, xlog = xlog, i = i)

    axes <- is.null(pars$axes)
    if(!axes) { axes <- pars$axes; pars$axes <- NULL }
    if(axes) {
	ax.pars <- pars[names(pars) %in% c("xaxt", "yaxt", "las",
					   "cex.axis", "col.axis")]
	if (is.null(show.names)) show.names <- n > 1
	if (show.names)
	    do.call("axis", c(list(side = 1 + horizontal,
				   at = at, labels = z$names), ax.pars))
	do.call("axis", c(list(side = 2 - horizontal), ax.pars))
    }
    do.call("title",
            pars[names(pars) %in% c("main", "cex.main", "col.main",
                                    "sub", "cex.sub", "col.sub",
                                    "xlab", "ylab", "cex.lab", "col.lab")])
    if(frame.plot)
	box()
    invisible(at)
}
