axis <- function(side, at=NULL, labels=TRUE, tick=TRUE, line=NA, pos=NA,
                 outer=FALSE, font=NA, vfont=NULL,
                 lty = "solid", lwd = 1, col=NULL, padj=NA, ...) {
    if (!is.null(vfont))
	vfont <- c(typeface = pmatch(vfont[1], Hershey$typeface) - 1,
		   fontindex= pmatch(vfont[2], Hershey$fontindex))
    if(is.null(col) && length(list(...)) && !is.null(fg <- list(...)$fg)) {
        ## help(par) `fg' says this should work
        col <- fg
    }
    .Internal(axis(side, at, labels, tick, line, pos, outer, font, vfont,
                   lty, lwd, col, padj, ...))
}

axTicks <- function(side, axp=NULL, usr=NULL, log=NULL) {
    ## Compute tickmark "at" values which axis(side) would use by default;
    ## using par("Xaxp") , par("usr") & par("Xlog") where X = x|y
    ## an R version of internal CreateAtVector()
    if(!(side <- as.integer(side)) %in% 1:4)
        stop("`side' must be in {1:4}")
    is.x <- side %% 2 == 1
    XY <- function(ch) paste(if(is.x) "x" else "y", ch, sep="")
    if(is.null(axp)) axp <- par(XY("axp"))
    else if(!is.numeric(axp) || length(axp) != 3) stop("invalid `axp'")
    if(is.null(log)) log <- par(XY("log"))
    else if(!is.logical(log) || any(is.na(log))) stop("invalid `log'")
    if(log && axp[3] > 0) { ## special log-scale axp[]
        if(!any((iC <- as.integer(axp[3])) == 1:3))
            stop("invalid positive axp[3]")
        if(is.null(usr)) usr <- par("usr")[if(is.x) 1:2 else 3:4]
        else if(!is.numeric(usr) || length(usr) != 2) stop("invalid `usr'")
        ii <- round(log10(axp[1:2]))
        x10 <- 10^((ii[1] - (iC >= 2)):ii[2])
	r <- switch(iC,				## axp[3]
		    x10,			## 1
		    c(outer(c(1,  5), x10))[-1],## 2
                    c(outer(c(1,2,5), x10))[-1])## 3
        r[usr[1] <= log10(r) & log10(r) <= usr[2]]
    } else { # linear
        seq(axp[1], axp[2], length = 1 + abs(axp[3]))
    }
}
