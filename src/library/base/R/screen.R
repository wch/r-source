split.screen <-
    function(figs,
	     screen = if(exists(".split.screens", envir=.GlobalEnv))
		      .split.cur.screen else 0,
	     erase = TRUE)
{
    first.split <- !exists(".split.screens", envir=.GlobalEnv)
    if (missing(figs))
	if (first.split)
	    return(FALSE)
	else
	    return(.split.valid.screens)
    if ((first.split && screen != 0) ||
	(!first.split && !(screen %in% .split.valid.screens)))
	stop("Invalid screen number\n")
    ## if figs isn't a matrix, make it one
    if (!is.matrix(figs)) {
	if (!is.vector(figs))
	    stop("figs must be a vector or a matrix with 4 columns\n")
	nr <- figs[1]
	nc <- figs[2]
	x <- seq(0, 1, len=nc+1)
	y <- seq(1, 0, len=nr+1)
	figs <- matrix(c(rep(x[-(nc+1)], nr), rep(x[-1], nr),
			 rep(y[-1], rep(nc, nr)),
			 rep(y[-(nr+1)], rep(nc, nr))),
		       nc=4)
    }
    num.screens <- nrow(figs)
    if (num.screens < 1)
	stop("figs must specify at least one screen\n")
    new.screens <- valid.screens <- cur.screen <- 0
    if (first.split) {
        if (erase) plot.new()
	split.par.list <- c("adj", "bty", "cex", "col", "crt", "err",
			    "font", "lab", "las", "lty",
			    "lwd", "mar", "mex", "mfg", "mgp",
			    "pch", "pty", "smo", "srt", "tck", "usr",
			    "xaxp", "xaxs", "xaxt", "xpd", "yaxp",
			    "yaxs", "yaxt", "fig")
	assign(".split.par.list", split.par.list, envir=.GlobalEnv)
	## save the current graphics state
	split.saved.pars <- par(split.par.list)
	split.saved.pars$fig <- NULL
	## NOTE: remove all margins when split screens
	split.saved.pars$omi <- par(omi=rep(0,4))$omi
	assign(".split.saved.pars", split.saved.pars, envir=.GlobalEnv)
	## set up the screen information
	split.screens <- vector(mode="list", length=num.screens)
	new.screens <- 1:num.screens
	for (i in new.screens) {
	    split.screens[[i]] <- par(split.par.list)
	    split.screens[[i]]$fig <- figs[i,]
	}
	valid.screens <- new.screens
	cur.screen <- 1
    }
    else {
	max.screen <- max(.split.valid.screens)
	new.max.screen <- max.screen + num.screens
	split.screens <- .split.screens
	## convert figs to portions of the specified screen
	total <- c(0,1,0,1)
	if (screen > 0)
	    total <- split.screens[[screen]]$fig
	for (i in 1:num.screens)
	    figs[i,] <- total[c(1,1,3,3)] +
		figs[i,]*rep(c(total[2]-total[1],
			       total[4]-total[3]),
			     c(2,2))
	new.screens <- (max.screen+1):new.max.screen
	for (i in new.screens) {
	    split.screens[[i]] <- par(.split.par.list)
	    split.screens[[i]]$fig <- figs[i-max.screen,]
	}
	valid.screens <- c(.split.valid.screens, new.screens)
	cur.screen <- max.screen+1
    }
    assign(".split.screens", split.screens, envir=.GlobalEnv)
    assign(".split.cur.screen", cur.screen, envir=.GlobalEnv)
    assign(".split.valid.screens", valid.screens, envir=.GlobalEnv)
    if (erase)
	erase.screen(0)
    par(.split.screens[[cur.screen]])
    return(new.screens)
}

screen <- function(n = .split.cur.screen, new = TRUE)
{
    if (!exists(".split.screens", envir=.GlobalEnv))
	return(FALSE)
    if (missing(n) && missing(new))
	return(.split.cur.screen)
    if (!(n %in% .split.valid.screens))
	stop("Invalid screen number\n")
    .split.screens[[.split.cur.screen]] <- par(.split.par.list)
    assign(".split.screens", .split.screens, envir=.GlobalEnv)
    assign(".split.cur.screen", n, envir=.GlobalEnv)
    par(.split.screens[[n]])
    if (new)
	erase.screen(n)
    invisible(n)
}

erase.screen <- function(n = .split.cur.screen)
{
    if (!exists(".split.screens", envir=.GlobalEnv))
	return(FALSE)
    if (!(n %in% .split.valid.screens) && n != 0)
	stop("Invalid screen number\n")
    old <- par(usr=c(0,1,0,1), mar=c(0,0,0,0),
	       fig = if (n > 0)
	       .split.screens[[n]]$fig
	       else
	       c(0,1,0,1),
	       xaxs="i", yaxs="i")
    on.exit(par(old))
    par(new=TRUE)
    plot.new()
    polygon(c(0,1,1,0), c(0,0,1,1), border=NA, col=0)
    par(new=TRUE)
    invisible()
}

close.screen <- function(n, all.screens=FALSE)
{
    if (!exists(".split.screens", envir=.GlobalEnv))
	return(FALSE)
    if (missing(n) && missing(all.screens))
	return(.split.valid.screens)
    if (all.screens || all(.split.valid.screens %in% n)) {
	par(.split.saved.pars)
	par(mfrow=c(1,1), new=FALSE)
	remove(".split.screens", ".split.cur.screen",
	       ".split.saved.pars", ".split.valid.screens",
	       ".split.par.list",
	       envir=.GlobalEnv)
	invisible()
    }
    else {
	assign(".split.valid.screens",
	       .split.valid.screens[-sort(match(n, .split.valid.screens))],
	       envir=.GlobalEnv)
	temp <- .split.cur.screen
	if (temp %in% n)
	    temp <- min(.split.valid.screens[.split.valid.screens>temp])
	if (temp > max(.split.valid.screens))
	    temp <- min(.split.valid.screens)
	screen(temp, new=FALSE)
	return(.split.valid.screens)
    }
}




