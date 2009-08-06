#  File src/library/graphics/R/axis.R
#  Part of the R package, http://www.R-project.org
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
#  http://www.r-project.org/Licenses/

axis <- function(side, at = NULL, labels = TRUE, tick = TRUE, line = NA,
                 pos = NA, outer = FALSE, font = NA,
                 lty = "solid", lwd = 1, lwd.ticks = lwd,
                 col = NULL, col.ticks = NULL,
                 hadj = NA, padj = NA, ...)
{
    ## we need to do this as the C code processes 'col' before '...'
    if(is.null(col) && length(list(...)) && !is.null(fg <- list(...)$fg)) {
        ## help(par) 'fg' says this should work
        col <- fg
    }
    ## watch out: some people do things like
    ## axis(1, at = my.at <- 10^(1L:5), labels = formatC(my.at, format="fg"))
    ## which depends on the order of evaluation of the args.
    .Internal(axis(side, at, as.graphicsAnnot(labels),
                   tick, line, pos, outer, font, lty, lwd, lwd.ticks,
                   col, col.ticks, hadj, padj, ...))
}


Axis <- function(x=NULL, at=NULL, ..., side, labels=NULL)
{
    if (!is.null(x)) UseMethod("Axis", x)
    else if (!is.null(at)) UseMethod("Axis", at)
    else axis(side=side, at=at, labels=labels, ...)
}

Axis.default <- function(x=NULL, at=NULL, ..., side, labels=NULL)
    axis(side=side, at=at, labels=labels, ...)


## Note that axTicks() can be used without any graphics device
## when (axp, usr, log) are specified.  However, axTicks() should
## *really* just call .Internal(createAtVector(axp,usr,nint,log))
axTicks <- function(side, axp = NULL, usr = NULL, log = NULL)
{
    ## Compute tickmark "at" values which axis(side) would use by default;
    ## using par("Xaxp") , par("usr") & par("Xlog") where X = x|y
    ## an R version of internal CreateAtVector()
    if(!(side <- as.integer(side)) %in% 1L:4L)
        stop("'side' must be in {1:4}")
    is.x <- side %% 2 == 1
    XY <- function(ch) paste(if(is.x) "x" else "y", ch, sep="")
    if(is.null(axp)) axp <- par(XY("axp"))
    else if(!is.numeric(axp) || length(axp) != 3) stop("invalid 'axp'")
    if(is.null(log)) log <- par(XY("log"))
    else if(!is.logical(log) || any(is.na(log))) stop("invalid 'log'")
    if(log && axp[3L] > 0) { ## special log-scale axp[]
        if(!any((iC <- as.integer(axp[3L])) == 1L:3L))
            stop("invalid positive 'axp[3]'")
        if(is.null(usr)) usr <- par("usr")[if(is.x) 1L:2L else 3L:4L]
        else if(!is.numeric(usr) || length(usr) != 2) stop("invalid 'usr'")
        ## need sorting for the case of reverse axes
        ii <- round(log10(sort(axp[1L:2L])))
        usr <- sort(usr)
        x10 <- 10^((ii[1L] - (iC >= 2L)):ii[2L])
	r <- switch(iC,				## axp[3L]
		    x10,			## 1
		    c(outer(c(1,  5), x10))[-1L],## 2
                    c(outer(c(1,2,5), x10))[-1L])## 3
        r[usr[1L] <= log10(r) & log10(r) <= usr[2L]]
    } else { # linear
        seq.int(axp[1L], axp[2L], length.out = 1L + abs(axp[3L]))
    }
}
