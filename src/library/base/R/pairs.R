pairs <- function(x, ...) UseMethod("pairs")

pairs.default <- function(x, labels, panel=points, main = NULL,
			  font.main=par("font.main"),
			  cex.main=par("cex.main"),  oma=NULL, ...)
{
    if(!is.matrix(x)) x <- data.matrix(x)
    if(!is.numeric(x)) stop("non-numeric argument to pairs")
    nc <- ncol(x)
    if(nc < 2) stop("only one column in the argument to pairs")
    if (missing(labels)) {
	labels <- dimnames(x)[[2]]
	if (is.null(labels))
	    labels <- paste("var", 1:nc)
    }
    if(is.null(oma)) {
        oma <- c(4, 4, 4, 4)
        if (!is.null(main)) oma[3] <- 6
    }
    opar <- par(mfrow = c(nc, nc), mar = rep(0.5, 4), oma = oma)
    on.exit(par(opar))
    for (i in 1:nc) for (j in 1:nc) {
	if (i == j) {
	    plot(x[, j], x[, i], xlab = "", ylab = "", axes = FALSE,
                 type = "n", ...)
	    box()
	    text(mean(par("usr")[1:2]), mean(par("usr")[3:4]), labels[i])
	}
	else {
	    plot(x[, j], x[, i], type="n", xlab = "", ylab = "", axes =
                 FALSE, ...)
	    box()
	    panel(x[, j], x[, i], ...)
	}
	if (j == 1 & 2 * floor(i/2) == i)
	    axis(2, xpd=NA)
	if (i == 1 & 2 * floor(j/2) == j)
	    axis(3, xpd=NA)
	if (j == nc & 2 * floor(i/2) != i)
	    axis(4, xpd=NA)
	if (i == nc & 2 * floor(j/2) != j)
	    axis(1, xpd=NA)
    }
    if (!is.null(main))
        mtext(main, 3, 3, TRUE, 0.5, cex = cex.main, font = font.main)
    invisible(NULL)
}

pairs.formula <- function(formula, data = NULL, subset, na.action, ...)
{
    if (missing(na.action))
        na.action <- getOption("na.action")
    m <- match.call(expand.dots = FALSE)
    if (is.matrix(eval(m$data, sys.frame(sys.parent()))))
        m$data <- as.data.frame(data)
    m$... <- NULL
    m[[1]] <- as.name("model.frame")
    mf <- eval(m, sys.frame(sys.parent()))
    pairs(mf, ...)
}
