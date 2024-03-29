#  File src/library/graphics/R/screen.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2023 The R Core Team
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  https://www.R-project.org/Licenses/


## An environment not exported from namespace:graphics used to
## store the split.screen settings
.SSenv <- new.env()

.SSname <- function(x) paste(x, dev.cur(), sep=":")
.SSget    <- function(x)           get(.SSname(x), envir=.SSenv, inherits=FALSE)
.SSexists <- function(x)        exists(.SSname(x), envir=.SSenv, inherits=FALSE)
.SSassign <- function(x, value) assign(.SSname(x), value, envir=.SSenv)
.SSenv$par.list <-
       c("xlog","ylog",
         "adj", "bty", "cex", "col", "crt", "err", "font", "lab",
         "las", "lty", "lwd", "mar", "mex", "mfg", "mgp", "pch",
         "pty", "smo", "srt", "tck", "usr", "xaxp", "xaxs", "xaxt", "xpd",
         "yaxp", "yaxs", "yaxt", "fig")

split.screen <-
    function(figs, screen, erase = TRUE)
{
    first.split <- !.SSexists("sp.screens")
    if(missing(screen))
        screen <- if(!first.split) .SSget("sp.cur.screen") else 0
    if(!first.split) .valid.screens <- .SSget("sp.valid.screens")
    if (missing(figs))
	if (first.split)
	    return(FALSE)
	else
	    return(.valid.screens)
    if ((first.split && screen != 0) ||
	(!first.split && !(screen %in% .valid.screens)))
	stop("invalid screen number")
    ## if figs isn't a matrix, make it one
    if (!is.matrix(figs)) {
	if (!is.vector(figs))
	    stop("'figs' must be a vector or a matrix with 4 columns")
	nr <- figs[1L]
	nc <- figs[2L]
	x <- seq.int(0, 1, length.out=nc+1)
	y <- seq.int(1, 0, length.out=nr+1)
	figs <- matrix(c(rep.int(x[-(nc+1)], nr), rep.int(x[-1L], nr),
			 rep.int(y[-1L], rep.int(nc, nr)),
			 rep.int(y[-(nr+1)], rep.int(nc, nr))),
		       ncol = 4)
    }
    num.screens <- nrow(figs)
    if (num.screens < 1)
	stop("'figs' must specify at least one screen")
    new.screens <- valid.screens <- cur.screen <- 0
    if (first.split) {
        if (erase) plot.new()
	## save the current graphics state
	split.saved.pars <- par(.SSenv$par.list)
	split.saved.pars$fig <- NULL
	## NOTE: remove all margins when split screens
	split.saved.pars$omi <- par(omi=rep.int(0,4))$omi
	.SSassign("sp.saved.pars", split.saved.pars)
	## set up the screen information
	split.screens <- vector(mode="list", length=num.screens)
	new.screens <- 1L:num.screens
	for (i in new.screens) {
	    split.screens[[i]] <- par(.SSenv$par.list)
	    split.screens[[i]]$fig <- figs[i,]
	}
	valid.screens <- new.screens
	cur.screen <- 1
    }
    else {
        if (erase) erase.screen(screen)
	max.screen <- max(.valid.screens)
	new.max.screen <- max.screen + num.screens
	split.screens <- .SSget("sp.screens")
	## convert figs to portions of the specified screen
	total <- c(0,1,0,1)
	if (screen > 0)
	    total <- split.screens[[screen]]$fig
	for (i in 1L:num.screens)
	    figs[i,] <- total[c(1,1,3,3)] +
		figs[i,]*rep.int(c(total[2L]-total[1L],
                                   total[4L]-total[3L]),
                                 c(2,2))
	new.screens <- (max.screen+1):new.max.screen
	for (i in new.screens) {
	    split.screens[[i]] <- par(.SSenv$par.list)
	    split.screens[[i]]$fig <- figs[i-max.screen,]
	}
	valid.screens <- c(.valid.screens, new.screens)
	cur.screen <- max.screen+1
    }
    .SSassign("sp.screens", split.screens)
    .SSassign("sp.cur.screen", cur.screen)
    .SSassign("sp.valid.screens", valid.screens)
    if(first.split) on.exit(close.screen(all.screens=TRUE))
    par(split.screens[[cur.screen]])
    on.exit()
    return(new.screens)
}

screen <- function(n = cur.screen, new = TRUE)
{
    if (!.SSexists("sp.screens"))
	return(FALSE)
    cur.screen <- .SSget("sp.cur.screen")
    if (missing(n) && missing(new))
	return(cur.screen)
    if (!(n %in% .SSget("sp.valid.screens")))
	stop("invalid screen number")
    split.screens <- .SSget("sp.screens")
    split.screens[[cur.screen]] <- par(.SSenv$par.list)
    .SSassign("sp.screens", split.screens)
    .SSassign("sp.cur.screen", n)
    par(split.screens[[n]])
    if (new)
	erase.screen(n)
    invisible(n)
}

erase.screen <- function(n = cur.screen)
{
    if (!.SSexists("sp.screens"))
	return(FALSE)
    cur.screen <- .SSget("sp.cur.screen")
    if (!(n %in% .SSget("sp.valid.screens")) && n != 0)
	stop("invalid screen number")
    old <- par(usr=c(0,1,0,1), mar=c(0,0,0,0),
	       fig = if (n > 0)
	       .SSget("sp.screens")[[n]]$fig
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
    if (!.SSexists("sp.screens"))
	return(FALSE)
    if (missing(n) && missing(all.screens))
	return(.SSget("sp.valid.screens"))
    valid.screens <- .SSget("sp.valid.screens")
    if (all.screens || all(valid.screens %in% n)) {
	par(.SSget("sp.saved.pars") )
	par(mfrow=c(1,1), new=FALSE)
	rm(list=.SSname(c("sp.screens", "sp.cur.screen", "sp.saved.pars", "sp.valid.screens")),
           envir=.SSenv)
	invisible()
    } else {
        valid.screens <- valid.screens[-sort(match(n, valid.screens))]
	.SSassign("sp.valid.screens", valid.screens)
	temp <- .SSget("sp.cur.screen")
	if (temp %in% n) {
            poss <- valid.screens[valid.screens>temp]
	    temp <- if(length(poss)) min(poss) else min(valid.screens)
        }
	screen(temp, new=FALSE)
	valid.screens
    }
}
