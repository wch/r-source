xy.coords <- function(x, y, xlab=NULL, ylab=NULL) {
	if(is.null(y)) {
		ylab<- xlab
		if(is.language(x)) {
			if(length(x) == 3 && deparse(x[[1]]) == '~') {
				ylab <- deparse(x[[2]])
				xlab <- deparse(x[[3]])
				y <- eval(x[[2]], sys.frame(sys.parent()))
				x <- eval(x[[3]], sys.frame(sys.parent()))
			}
			else stop("invalid first argument")
		}
		else if(is.ts(x)) {
			if(is.matrix(x)) y <- x[,1]
			else y <- x
			x <- time(x)
			xlab <- "Time"
		}
		else if(is.complex(x)) {
			y <- Im(x)
			x <- Re(x)
			xlab <- paste("Re(", ylab, ")", sep="")
			ylab <- paste("Im(", ylab, ")", sep="")
		}
		else if(is.matrix(x) || is.data.frame(x)) {
			x <- data.matrix(x)
			if(ncol(x) == 1) {
				xlab <- "Index"
				y <- x[,1]
				x <- 1:length(y)
			}
			else {
				colnames <- dimnames(x)[[2]]
				if(is.null(colnames)) {
					xlab <- paste(ylab,"[,1]",sep="")
					ylab <- paste(ylab,"[,2]",sep="")
				}
				else {
					xlab <- colnames[1]
					ylab <- colnames[2]
				}
				y <- x[,2]
				x <- x[,1]
			}
		}
		else if(is.list(x)) {
			xlab <- paste(ylab,"$x",sep="")
			ylab <- paste(ylab,"$y",sep="")
			y <- x[["y"]]
			x <- x[["x"]]
		}
		else {
			if(is.factor(x)) x <- as.numeric(x)
			xlab <- "Index"
			y <- x
			x <- 1:length(x)
		}
	}
	else if(length(x) != length(y)) stop("x and y lengths differ")
	return(list(x=as.real(x), y=as.real(y), xlab=xlab, ylab=ylab))
}

plot <- function(x, ...)
UseMethod("plot")

plot.default <-
function (x, y=NULL, type="p", main=NULL, col=par("fg"), bg=NA,
          pch=par("pch"), xlim=NULL, ylim=NULL, log="", axes=TRUE,
          frame.plot=axes, panel.first=NULL, panel.last=NULL,
          ann=par("ann"), xlab=NULL, ylab=NULL, cex=par("cex"),
          lty=par("lty"), lwd=par("lwd"), asp=NA, ...)
{
 xlabel <- if (!missing(x)) deparse(substitute(x))	else NULL
 ylabel <- if (!missing(y)) deparse(substitute(y))	else NULL
 xy <- xy.coords(x, y, xlabel, ylabel)
 xlab <- if (is.null(xlab)) xy$xlab	else xlab
 ylab <- if (is.null(ylab)) xy$ylab	else ylab
 xlim <- if (is.null(xlim)) range(xy$x, finite=TRUE) else xlim
 ylim <- if (is.null(ylim)) range(xy$y, finite=TRUE) else ylim
 plot.new()
 plot.window(xlim, ylim, log, asp, ...)
 panel.first
 plot.xy(xy, type, col=col, pch=pch, cex=cex, bg=bg, lty=lty, lwd=lwd, ...)
 panel.last
 pars <- list(...)
 if (axes) {
	axis(1, pars=pars)
	axis(2, pars=pars)
 }
 if (frame.plot)
	box(...)
 if (ann)
	title(main=main, xlab=xlab, ylab=ylab, pars=pars)
 invisible()
}

plot.factor <-
function(x, y, ...) {
  if (missing(y))
    barplot(table(x), ...)
  else if (is.numeric(y))
    boxplot(y ~ x, ...)
  else NextMethod("plot")
}

plot.formula <-
function(formula, data = NULL, subset, na.action, ..., ask = TRUE) {
  if (missing(na.action)) na.action <- options()$na.action
  m <- match.call(expand.dots = F)
  if (is.matrix(eval(m$data, sys.parent())))
    m$data <- as.data.frame(data)
  m$... <- NULL
  m[[1]] <- as.name("model.frame")
  mf <- eval(m, sys.parent())
  response <- attr(attr(mf, "terms"), "response")
  if (response) {
    varnames <- names(mf)
    y <- mf[[response]]
    ylab <- varnames[response]
    if (length(varnames) > 2) {
      opar <- par(ask = ask)
      on.exit(par(opar))
    }
    for (i in varnames[-response])
      plot(mf[[i]], y, xlab = i, ylab = ylab, ...)
  }
  else plot.data.frame(mf)
}

plot.xy <-
function(xy, type, pch=1, lty="solid", col=par("fg"), bg=NA, cex=1, ...)
	.Internal(plot.xy(xy, type, pch, lty, col, bg=bg, cex=cex, ...))

plot.new <- function(ask=NA)
	.Internal(plot.new(ask))

frame <- plot.new
