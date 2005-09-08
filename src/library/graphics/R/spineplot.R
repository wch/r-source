## Spine plots/Spinograms
## written by Achim Zeileis <Achim.Zeileis@R-project.org>

spineplot <- function(x, ...) {
  UseMethod("spineplot")
}

spineplot.formula <- function(formula, data = list(),
  breaks = NULL, tol.ylab = 0.05, off = NULL,
  col = NULL, main = "", xlab = NULL, ylab = NULL, ...)
{
  ## extract x, y from formula
  mf <- model.frame(formula, data = data)
  if(NCOL(mf) != 2) stop("`formula' should specify exactly two variables")
  y <- mf[,1]
  if(!is.factor(y)) stop("dependent variable should be a factor")
  x <- mf[,2]	
  if(is.null(xlab)) xlab <- names(mf)[2]
  if(is.null(ylab)) ylab <- names(mf)[1]
     
  spineplot(x, y,
    breaks = breaks, tol.ylab = tol.ylab, off = off,
    col = col, main = main, xlab = xlab, ylab = ylab, ...)
}

spineplot.default <- function(x, y = NULL,
  breaks = NULL, tol.ylab = 0.05, off = NULL,
  col = NULL, main = "", xlab = NULL, ylab = NULL, ...)
{
  ## either supply a 2-way table (i.e., both y and x are categorical)
  ## or two variables (y has to be categorical - x can be categorical or numerical)
  if(missing(y)) {
    if(length(dim(x)) != 2) stop("a 2-way table has to be specified")
    tab <- x
    x.categorical <- TRUE
    if(is.null(xlab)) xlab <- names(dimnames(tab))[1]
    if(is.null(ylab)) ylab <- names(dimnames(tab))[2]
    xnam <- dimnames(tab)[[1]]
    ynam <- dimnames(tab)[[2]]
    ny <- NCOL(tab)
    nx <- NROW(tab)
  } else {
    if(!is.factor(y)) stop("dependent variable should be a factor")
    x.categorical <- is.factor(x)
    if(!x.categorical) stopifnot(is.numeric(x), is.vector(x))
    if(is.null(xlab)) xlab <- deparse(substitute(x))
    if(is.null(ylab)) ylab <- deparse(substitute(y))
    if(x.categorical) {
      tab <- table(x, y)
      xnam <- levels(x)
      nx <- NROW(tab)
    }
    ynam <- levels(y)
    ny <- length(ynam)
  }

  ## graphical parameters
  if(is.null(col)) col <- gray.colors(ny)
  col <- rep(col, length.out = ny)
  off <- if(!x.categorical) 0 else if(is.null(off)) 0.02 else off/100

  if(x.categorical) {
    ## compute rectangle positions on x axis
    xat <- c(0, cumsum(prop.table(margin.table(tab, 1)) + off))
  } else {
    ## compute breaks for x
    if(is.null(breaks)) breaks <- list()
    if(!is.list(breaks)) breaks <- list(breaks = breaks)
    breaks <- c(list(x = x), breaks)
    breaks$plot <- FALSE
    breaks <- do.call("hist", breaks)$breaks
    ## categorize x
    x1 <- cut(x, breaks = breaks, include.lowest = TRUE)
    ## compute rectangle positions on x axis
    xat <- c(0, cumsum(prop.table(table(x1))))
    ## construct table
    tab <- table(x1, y)
    nx <- NROW(tab)
  }
  
  ## compute rectangle positions on y axis
  yat <- rbind(0, apply(prop.table(tab, 1), 1, cumsum))
  
  ## setup plot
  plot(0, 0, xlim = c(0, 1 + off * (nx-1)), ylim = c(0, 1), type = "n", axes = FALSE,
       xaxs = "i", yaxs = "i", main = main, xlab = xlab, ylab = ylab)

  ## compute coordinates
  ybottom <- as.vector(yat[-(ny+1),])
  ytop <- as.vector(yat[-1,])
  xleft <- rep(xat[1:nx], rep(ny, nx))
  xright <- rep(xat[2:(nx+1)] - off, rep(ny, nx))
  col <- rep(col, nx)

  ## plot rectangles
  rect(xleft, ybottom, xright, ytop, col = col, ...)

  ## axes
  ## 1: either numeric or level names
  if(x.categorical)
    axis(1, at = (xat[1:nx] + xat[2:(nx+1)] - off)/2, labels = xnam, tick = FALSE)
  else
    axis(1, at = xat, labels = breaks)
    
  ## 2: axis with level names of y
  yat <- yat[,1]
  equidist <- any(diff(yat) < tol.ylab)
  yat <- if(equidist) seq(1/(2*ny), 1-1/(2*ny), by = 1/ny)
           else (yat[-1] + yat[-length(yat)])/2
  axis(2, at = yat, labels = ynam, tick = FALSE)
  
  ## 3: none
  ## 4: simple numeric  
  axis(4)
  
  ## return table visualized
  names(dimnames(tab)) <- c(xlab, ylab)
  invisible(tab)  
}
