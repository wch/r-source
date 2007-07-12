## Author: Martin Maechler, Date: 27 Jun 97

matpoints <-
    function(x, y,  type = "p", lty=1:5, lwd = 1, pch=NULL, col=1:6, ...)
    matplot(x=x, y=y, type = type, lty=lty, lwd=lwd, pch=pch, col=col,
	    add=TRUE, ...)
matlines  <-
    function(x, y, type = "l", lty=1:5, lwd = 1, pch=NULL, col=1:6, ...)
    matplot(x=x, y=y, type = type, lty=lty, lwd=lwd, pch=pch, col=col,
	    add=TRUE, ...)

matplot <- function(x, y, type = "p",
		    lty = 1:5, lwd = 1, pch=NULL, col=1:6, cex=NULL, bg=NA,
		    xlab=NULL, ylab=NULL, xlim=NULL, ylim=NULL,
		    ..., add= FALSE, verbose = getOption("verbose"))
{
    paste.ch <- function(chv) paste('"',chv,'"', sep="", collapse=" ")
    str2vec <- function(string) {
	if(nchar(string, type="c")[1] > 1)
	    strsplit(string[1], NULL)[[1]]
	else string
    }
    ## These from plot.default :
    xlabel <- if (!missing(x)) deparse(substitute(x))# else NULL
    ylabel <- if (!missing(y)) deparse(substitute(y))
    ##
    if(missing(x)) {
	if(missing(y)) stop("must specify at least one of 'x' and 'y'")
	else x <- 1:NROW(y)
    } else if(missing(y)) {
	y <- x;		ylabel <- xlabel
	x <- 1:NROW(y); xlabel <- ""
    }
    kx <- ncol(x <- as.matrix(x))
    ky <- ncol(y <- as.matrix(y))
    n <- nrow(x)
    if(n != nrow(y)) stop("'x' and 'y' must have same number of rows")

    if(kx > 1 && ky > 1 && kx != ky)
	stop("'x' and 'y' must have only 1 or the same number of columns")
    if(kx == 1) x <- matrix(x, nrow = n, ncol = ky)
    if(ky == 1) y <- matrix(y, nrow = n, ncol = kx)
    k <- max(kx,ky)## k == kx == ky

    type <- str2vec(type)
    if(is.null(pch))
	pch <- c(paste(c(1:9,0)),letters)[1:k]
    else if(is.character(pch))
	pch <- str2vec(pch)
    ## else pch is numeric supposedly
    if(verbose)
	message("matplot: doing ", k, " plots with ",
	    paste(" col= (", paste.ch(col), ")", sep=''),
	    paste(" pch= (", paste.ch(pch), ")", sep=''),
	    " ...\n", domain=NA)
    ii <- match("log", names(xargs <- list(...)), nomatch = 0)
    log <- if (ii != 0) xargs[[ii]]
    xy <- xy.coords(x, y, xlabel, ylabel, log=log)
    xlab <- if (is.null(xlab)) xy$xlab else xlab
    ylab <- if (is.null(ylab)) xy$ylab else ylab
    xlim <- if (is.null(xlim)) range(xy$x[is.finite(xy$x)]) else xlim
    ylim <- if (is.null(ylim)) range(xy$y[is.finite(xy$y)]) else ylim
    if(length(type)< k) type<- rep(type,length.out = k)
    if(length(lty) < k) lty <- rep(lty, length.out = k)
    if(length(lwd) < k) lwd <- rep(lwd, length.out = k)
    if(length(pch) < k) pch <- rep(pch, length.out = k)
    if(length(col) < k) col <- rep(col, length.out = k)
    if(length(bg) < k)	bg  <- rep(bg,	length.out = k)
    if(length(cex) < k) cex <- rep(cex, length.out = k)
    ii <- 1:k
    if(!add) {
	ii <- ii[-1]
	plot(x[,1],y[,1], type=type[1], xlab=xlab, ylab=ylab,
	     xlim = xlim, ylim = ylim, lty=lty[1], lwd=lwd[1],
	     pch=pch[1], col=col[1], cex=cex[1], bg=bg[1], ...)
    }
    for (i in ii) {
	lines(x[,i], y[,i], type=type[i], lty=lty[i],
	      lwd=lwd[i], pch=pch[i], col=col[i], cex=cex[i], bg=bg[i])
    }
}
