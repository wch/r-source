boxplot <- function(x, ..., range=1.5, width=NULL, varwidth=FALSE,
	notch=FALSE, names.x, data=sys.frame(sys.parent()),
	plot=TRUE, border=par("fg"), col=NULL, log="", pars=NULL)
{
	args <- list(x,...)
	namedargs <- if(!is.null(attributes(args)$names))
		attributes(args)$names != ""
	else
		rep(FALSE, length=length(args))

	pars <- c(args[namedargs], pars)

	groups <- if(is.language(x)) {
		if(length(x) == 3 && deparse(x[[1]]) == '~') {
			groups <- eval(x[[3]], data)
			x <- eval(x[[2]], data)
			split(x, groups)
		}
		else stop("invalid first argument")
	 }
	 else {
		groups <- args[!namedargs]
		if (length(groups) == 1 && is.list(x)) x else groups
	 }
	n <- length(groups)
	if(!missing(names.x)) attr(groups, "names") <- names.x
	else if(is.null(attr(groups, "names"))) attr(groups, "names") <- 1:n
	for(i in 1:n)
		groups[i] <- list(boxplot.stats(groups[[i]], range))
	if(plot) {
		bxp(groups, width, varwidth=varwidth, notch=notch,
			border=border, col=col, log=log, pars=pars)
		invisible(groups)
	}
	else groups
}

boxplot.stats <- function(x, coef = 1.5)
{
	nna <- !is.na(x)
	n <- length(nna)# including +/- Inf
	stats <- fivenum(x, na.rm=TRUE)
	iqr <- diff(stats[c(2, 4)])
	out <- x < (stats[2]-coef*iqr) | x > (stats[4]+coef*iqr)
	if(coef > 0) stats[c(1, 5)] <- range(x[!out], na.rm=TRUE)
	conf <- stats[3]+c(-1.58, 1.58)*diff(stats[c(2, 4)])/sqrt(n)
	list(stats=stats, n=n, conf=conf, out=x[out&nna])
}

bxp <- function(z, notch=FALSE, width=NULL, varwidth=FALSE,
		notch.frac = 0.5,
		border=par("fg"), col=NULL, log="", pars=NULL, ...)
{
 bplt <- function(x, wid, stats, out, conf, notch, border, col)
 {
	## Draw single box plot.
	pars <- c(pars, list(...))# from bxp(...).

	if(!any(is.na(stats))) {
		## stats = +/- Inf:  polygon & segments should handle
		wid <- wid/2
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

 n <- length(z)
 limits <- numeric(0)
 nmax <- 0
 for(i in 1:n) {
	nmax <- max(nmax,z[[i]]$n)
	limits <- range(limits, z[[i]]$stats, z[[i]]$out, finite=TRUE)
 }

 width <- if (!is.null(width)) {
		if (length(width) != n | any(is.na(width)) | any(width <= 0))
			stop("invalid boxplot widths")
		0.8 * width/max(width)
	}
	else if (varwidth) 0.8 * sqrt(unlist(lapply(z, "[[", "n"))/nmax)
	else if (n == 1) 0.4
	else rep(0.8, n)

 ylim <- if(is.null(pars$ylim)) limits else pars$ylim
 if(missing(border) || length(border)==0)
	border <- par("fg")

 plot.new()
 plot.window(xlim=c(0.5,n+0.5), ylim=ylim, log=log)

 for(i in 1:n)
	 bplt(i, wid=width[i],
	      stats= z[[i]]$stats,
	      out  = z[[i]]$out,
	      conf = z[[i]]$conf,
	      notch= notch,
	      border=border[(i-1)%%length(border)+1],
	      col=if(is.null(col)) col else col[(i-1)%%length(col)+1])

 if(is.null(pars$axes) || pars$axes) {
         if(n > 1) axis(1, at=1:n, labels=names(z))
         axis(2)
 }
 do.call("title", pars)
 box()
 invisible(1:n)
}
