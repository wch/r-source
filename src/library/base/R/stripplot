# Dotplots a la Box, Hunter and Hunter

stripplot <- function(x, method="overplot", jitter=0.1, offset=1/3,
		vertical=FALSE, group.names,
		xlim, ylim, main="", ylab="", xlab="",
		pch=0, col=par("fg"), cex=par("cex"))
{
	method <- pmatch(method, c("overplot", "jitter", "stack"))[1]
	if(is.na(method) || method==0)
		error("invalid plotting method")
	if(is.language(x)) {
		if(length(x) == 3 && deparse(x[[1]]) == '~') {
			groups <- eval(x[[3]], sys.frame(sys.parent())) 
			x <- eval(x[[2]], sys.frame(sys.parent()))
			groups <- split(x, groups)
		}
		else stop("invalid first argument")
	}
	else if(is.list(x)) {
		groups <- x
	}
	else if(is.numeric(x)) {
		groups <- list(x)
	}
	n <- length(groups)
	if(!missing(group.names)) attr(groups, "names") <- group.names
	else if(is.null(attr(groups, "names"))) attr(groups, "names") <- 1:n

	dlim <- rep(NA, 2)
	for(i in groups)
		dlim <- range(dlim, i, na.rm=T)
	glim <- c(1, n)
	if(method == 2) {
		if(n == 1) glim <- glim + c(-5, 5) * jitter
		else glim <- glim + c(-2, 2) * jitter
	}
	if(method == 3) {
		if(n == 1) glim <- glim + c(-1,1)
		else glim <- glim + c(0, 0.5)
	}
	if(missing(xlim)) {
		if(vertical) xlim <- glim
		else xlim <- dlim
	}
	if(missing(ylim)) {
		if(vertical) ylim <- dlim
		else ylim <- glim
	}
	plot(xlim, ylim, type="n", ann=FALSE, axes=FALSE)
	box()
	if(vertical) {
		if(n > 1) axis(1, at=1:n, lab=names(groups))
		axis(2)
	}
	else {
		axis(1)
		if(n > 1) axis(2, at=1:n, lab=names(groups))
	}

	if(vertical) csize <- cex*xinch(par("cin")[1])
	else csize <- cex*yinch(par("cin")[2])
	f <- function(x) seq(length(x))
	for(i in 1:length(groups)) {
		x <- groups[[i]]
		y <- rep(i,length(x))
		if(method == 2) y <- y + runif(length(y), -jitter, jitter)
		if(method == 3) {
			xg <- split(x, factor(x))
			xo <- lapply(xg, f)
			x <- unlist(xg)
			y <- y + (unlist(xo) - 1) * offset * csize
		}
		if(vertical) points(y, x, col=col[(i - 1)%%length(col) + 1],
			pch=pch[(i - 1)%%length(pch) + 1], cex=cex)
		else points(x, y, col=col[(i - 1)%%length(col) + 1],
			pch=pch[(i - 1)%%length(pch) + 1], cex=cex)
	}
	title(main=main, xlab=xlab, ylab=ylab)
}
