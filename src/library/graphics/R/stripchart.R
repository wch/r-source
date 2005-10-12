## Dotplots a la Box, Hunter and Hunter

stripchart <-
function(x, method="overplot", jitter=0.1, offset=1/3, vertical=FALSE,
	 group.names, add = FALSE, at = NULL,
	 xlim=NULL, ylim=NULL, main="", ylab="", xlab="",
	 log="", pch=0, col=par("fg"), cex=par("cex"))
{
    method <- pmatch(method, c("overplot", "jitter", "stack"))[1]
    if(is.na(method) || method==0)
	stop("invalid plotting method")
    groups <-
	if(is.language(x)) {
	    if (inherits(x, "formula") && length(x) == 3) {
		groups <- eval(x[[3]], parent.frame())
		x <- eval(x[[2]], parent.frame())
		split(x, groups)
	    }
	}
	else if(is.list(x)) x
	else if(is.numeric(x)) list(x)
    if(0 == (n <- length(groups)))
	stop("invalid first argument")
    if(!missing(group.names))
	attr(groups, "names") <- group.names
    else if(is.null(attr(groups, "names")))
	attr(groups, "names") <- 1:n
    if(is.null(at))
	at <- 1:n
    else if(length(at) != n)
	stop(gettextf("'at' must have length equal to the number %d of groups",
                      n), domain = NA)
    if(!add) {
	dlim <- c(NA, NA)
	for(i in groups)
	    dlim <- range(dlim, i[is.finite(i)], na.rm = TRUE)
	glim <- c(1,n)# in any case, not range(at)
	if(method == 2) { # jitter
	    glim <- glim + jitter * if(n == 1) c(-5, 5) else c(-2, 2)
	} else if(method == 3) { # stack
	    glim <- glim + if(n == 1) c(-1,1) else c(0, 0.5)
	}
	if(is.null(xlim))
	    xlim <- if(vertical) glim else dlim
	if(is.null(ylim))
	    ylim <- if(vertical) dlim else glim
	plot(xlim, ylim, type="n", ann=FALSE, axes=FALSE, log=log)
	box()
	if(vertical) {
	    if(n > 1) axis(1, at=at, lab=names(groups))
	    Axis(x, side = 2)
	}
	else {
	    Axis(x, side = 1)
	    if(n > 1) axis(2, at=at, lab=names(groups))
	}
    }
    csize <- cex*
	if(vertical) xinch(par("cin")[1]) else yinch(par("cin")[2])
    f <- function(x) seq(length=length(x))
    for(i in 1:n) {
	x <- groups[[i]]
	y <- rep.int(at[i], length(x))
	if(method == 2) ## jitter
	    y <- y + runif(length(y), -jitter, jitter)
	else if(method == 3) { ## stack
	    xg <- split(x, factor(x))
	    xo <- lapply(xg, f)
	    x <- unlist(xg, use.names=FALSE)
	    y <- rep.int(at[i], length(x)) +
		(unlist(xo, use.names=FALSE) - 1) * offset * csize
	}
	if(vertical) points(y, x, col=col[(i - 1)%%length(col) + 1],
			    pch=pch[(i - 1)%%length(pch) + 1], cex=cex)
	else points(x, y, col=col[(i - 1)%%length(col) + 1],
		    pch=pch[(i - 1)%%length(pch) + 1], cex=cex)
    }
    title(main=main, xlab=xlab, ylab=ylab)
}
