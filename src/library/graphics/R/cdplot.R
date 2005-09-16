## CD plots
## written by Achim Zeileis <Achim.Zeileis@R-project.org>

cdplot <- function(x, ...) {
  UseMethod("cdplot")
}

cdplot.formula <-
function(formula, data = list(),
         plot = TRUE, tol.ylab = 0.05,
         bw = "nrd0", n = 512, from = NULL, to = NULL,
         col = NULL, border = 1, main = "", xlab = NULL, ylab = NULL,
         yaxlabels = NULL, xlim = NULL, ylim = c(0, 1), ...)
{
    ## extract x, y from formula
    mf <- model.frame(formula, data = data)
    if(NCOL(mf) != 2)
        stop("'formula' should specify exactly two variables")
    y <- mf[,1]
    if(!is.factor(y))
        stop("dependent variable should be a factor")
    x <- mf[,2]	
    if(!is.numeric(x))
        stop("explanatory variable should be numeric")

    ## graphical parameters
    if(is.null(xlab)) xlab <- names(mf)[2]
    if(is.null(ylab)) ylab <- names(mf)[1]
    if(is.null(yaxlabels)) yaxlables <- levels(y)
    
    ## call default interface

    cdplot(x, y, plot = plot, tol.ylab = tol.ylab, bw = bw, n = n,
           from = from, to = to, col = col, border = border, main = main,
           xlab = xlab, ylab = ylab, yaxlabels = yaxlabels, xlim = xlim,
           ylim = ylim, ...)
}

cdplot.default <-
function(x, y,
         plot = TRUE, tol.ylab = 0.05,
         bw = "nrd0", n = 512, from = NULL, to = NULL,
         col = NULL, border = 1, main = "", xlab = NULL, ylab = NULL,
         yaxlabels = NULL, xlim = NULL, ylim = c(0, 1), ...)
{
    ## check x and y
    if(!is.numeric(x)) stop("explanatory variable should be numeric")
    if(!is.factor(y)) stop("dependent variable should be a factor")

    ## graphical parameters
    if(is.null(xlab)) xlab <- deparse(substitute(x))
    if(is.null(ylab)) ylab <- deparse(substitute(y))
    if(is.null(col)) col <- gray.colors(length(levels(y)))
    col <- rep(col, length.out = length(levels(y)))
    if(is.null(yaxlabels)) yaxlabels <- levels(y)
    
    ## unconditional density of x
    dx <- if(is.null(from) & is.null(to)) density(x, bw = bw, n = n, ...)
    else density(x, bw = bw, from = from, to = to, n = n, ...)	  
    x1 <- dx$x

    ## setup conditional values
    ny <- length(levels(y))
    yprop <- cumsum(prop.table(table(y)))
    y1 <- matrix(rep(0, n*(ny-1)), nrow = (ny-1))

    ## setup return value
    rval <- list()
    
    for(i in 1:(ny-1)) {
        dxi <- density(x[y %in% levels(y)[1:i]], bw = dx$bw, n = n, from = min(dx$x), to = max(dx$x), ...)
        y1[i,] <- dxi$y/dx$y * yprop[i]
        rval[[i]] <- approxfun(x1, y1[i,], rule = 2)
    }
    names(rval) <- levels(y)[1:(ny-1)]

    ## use known ranges
    y1 <- rbind(0, y1, 1)
    y1 <- y1[,which(x1 >= min(x) & x1 <= max(x))]
    x1 <- x1[x1 >= min(x) & x1 <= max(x)]

    if(is.null(xlim)) xlim <- range(x1)

    ## plot polygons
    if(plot) {
        plot(0, 0, xlim = xlim, ylim = ylim, type = "n", axes = FALSE,
             xaxs = "i", yaxs = "i", xlab = xlab, ylab = ylab)
        for(i in 1:(NROW(y1)-1)) {
            polygon(c(x1, rev(x1)), c(y1[i+1,], rev(y1[i,])), col = col[i], border = border)
        }
        axis(1)
    
        equidist <- any(diff(y1[,1]) < tol.ylab)
        if(equidist)
            axis(2, at = seq(1/(2*ny), 1-1/(2*ny), by = 1/ny), labels = yaxlabels, tick = FALSE)
        else
            axis(2, at = (y1[-1,1] + y1[-NROW(y1), 1])/2, labels = yaxlabels, tick = FALSE)
        axis(4)
        box()
    }
  
    ## return conditional density functions
    invisible(rval)
}

