legend <- 
function (x, y, legend, fill, col = "black", lty, lwd, pch, bty = "o", 
            bg = par("bg"), cex = 1, xjust = 0, yjust = 1, x.intersp = NULL, 
            y.intersp = NULL, text.width = NULL, merge = FALSE) 
{
  xlog <- par("xlog")
  ylog <- par("ylog")
  if (xlog) x <- log10(x)
  if (ylog) y <- log10(y)
  cin <- par("cin")
  #-- the 'effective' cex
  Cex <- cex * par("cex")
  if (is.null(x.intersp)) x.intersp <- min(4, 1.8 + 1.2 * Cex^-1.25)
  ##- if(DEBUG) cat('Cex=',formatC(Cex),' ==>  x.intersp=',
  ##-        format(x.intersp),'; y.intersp=', format(y.intersp),"\n")
  if (is.null(y.intersp)) 
    y.intersp <- min(2, 0.2 + Cex^-1.25)
  xchar <- Cex * xinch(cin[1])
  yextra <- Cex * yinch(cin[2]) * (y.intersp - 1)
  ychar <- max(c(strheight(legend, u = "user", cex = cex), 
                 Cex * yinch(cin[2]))) + yextra
  ## size of filled boxes:
  xbox <- Cex * xinch(cin[1]) * 0.8
  ybox <- Cex * yinch(cin[2]) * 0.8
  n.leg <- length(legend)
  ## -- (w,h) := (width,height) of the box to draw -- computed stepwise...
  w <- if (is.null(text.width)) {
    max(strwidth(legend, u = "user", cex = cex))
  }
  else {
    if (is.numeric(text.width) && text.width >= 0) text.width
    else stop("text.width must be numeric, >= 0")
  }
  w <- 2 * xchar + w
  h <- (n.leg + 1) * ychar
  if (missing(y)) {
    if (is.list(x)) {
      y <- x$y
      x <- x$x
    }
    else stop("missing y")
  }
  if (!is.numeric(x) || !is.numeric(y)) stop("non-numeric coordinates")
  if (length(x) <= 0 || length(x) != length(y)) stop("differing coordinate lengths")
  if (length(x) != 1) {
    x <- mean(x)
    y <- mean(y)
    xjust <- 0.5
    yjust <- 0.5
  }
  if (!missing(fill)) w <- w + xbox + xchar
  if (!missing(pch)) {
    if (is.character(pch) && nchar(pch) > 1) {
      np <- nchar(pch)
      pch <- substr(rep(pch[1], np), 1:np, 1:np)
    }
    if (!merge) w <- w + x.intersp/2 * xchar
  }
  if (!missing(lty)) 
    if (!merge) w <- w + x.intersp * xchar
  ## (w,h) are now the final box width/height. --> Adjust (x,y) :
  if (merge) w <- w + x.intersp * xchar
  left <- x - xjust * w
  top <- y + (1 - yjust) * h
  right <- left + w
  bottom <- top - h
  if (xlog) {
    left <- 10^left
    right <- 10^right
  }
  if (ylog) {
    top <- 10^top
    bottom <- 10^bottom
  }
  ## (xt[],yt[]) := 'current' vectors of (x/y) legend text
  if (bty != "n") rect(left, top, right, bottom, col = bg)
  if(xlog) left <- log10(left)
  if(ylog) top <- log10(top)
  xt <- rep(left, n.leg) + xchar
  yt <- top - (1:n.leg) * ychar
  if (!missing(fill)) {
    #- draw filled boxes -------------
    xx <- cbind(xt, xt + xbox)
    if (xlog) xx <- 10^xx
    yy <- yt + cbind(rep(-0.5, n.leg), 0.5) * ybox
    if (ylog) yy <- 10^yy
    rect(xx[, 1], yy[, 1], xx[, 2], yy[, 2], col = fill)
    xt <- xt + xbox + xchar
  }
  col <- rep(col, length.out = n.leg)
  if (!missing(pch)) {
    #- draw points -------------------
    pch <- rep(pch, length.out = n.leg)
    ok <- (is.character(pch) | pch > 0)
    x1 <- (xt + ifelse(merge, 0, 0.25) * xchar)[ok]
    if (xlog) x1 <- 10^x1
    y1 <- yt[ok]
    if (ylog) y1 <- 10^y1
    points(x1, y1, pch = pch[ok], col = col[ok], cex = cex)
    if (!merge) xt <- xt + x.intersp/2 * xchar
  }
  if ((!missing(lty) && any(lty > 0))||(!missing(lwd))) {
    #- draw lines -------
    if(missing(lty)) lty <- 1
    if(missing(lwd)) lwd <- 1
    lty <- rep(lty, length.out = n.leg)
    lwd <- rep(lwd, length.out = n.leg)
    ok <- lty > 0
    x.off <- if (merge) -0.8 else 0
    xx <- cbind(xt + x.off * xchar, xt + (2 + x.off) * xchar)
    if (xlog) xx <- 10^xx
    y1 <- yt
    if (ylog) y1 <- 10^y1
    olwd <- par("lwd")
    for(i in seq(along=lty))
      if(ok[i]) {
        lines(xx[i,], rep(y1[i], 2), lty = lty[i], col = col[i], lwd=lwd[i])
    }
    if (!merge) xt <- xt + 3 * xchar
  }
  if (merge) xt <- xt + x.intersp * xchar
  if (xlog) xt <- 10^xt
  ## adj = (x,y) text-box adjustment
  if (ylog) yt <- 10^yt
  text(xt, yt, labels = legend, adj = c(0, 0.3 * y.intersp), cex = cex)
}

