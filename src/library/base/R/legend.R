legend <-
function (x, y, legend, fill, col = "black", lty, lwd, pch, bty = "o",
	bg = par("bg"), cex = 1, xjust = 0, yjust = 1, x.intersp = NULL,
        y.intersp = NULL, text.width = NULL, merge = FALSE)
{
  if (missing(y)) {
    if (is.list(x)) { y <- x$y; x <- x$x } else stop("missing y")
  }
  if (!is.numeric(x) || !is.numeric(y))
    stop("non-numeric coordinates")
  if (length(x) <= 0 || length(x) != length(y))
    stop("differing coordinate lengths")

  xlog <- par("xlog")
  ylog <- par("ylog")

  rect2 <- function(left, top, dx, dy, ...) {
    r <- left + dx; if(xlog) { left <- 10^left; r <- 10^r }
    b <- top  - dy; if(ylog) {  top <- 10^top;  b <- 10^b }
    rect(left, top, r, b, ...)
  }
  segments2 <- function(x1, y1, dx, dy, ...) {
    x2 <- x1 + dx; if(xlog) { x1 <- 10^x1; x2 <- 10^x2 }
    y2 <- y1 + dy; if(ylog) { y1 <- 10^y1; y2 <- 10^y2 }
    segments(x1, y1, x2, y2, ...)
  }
  points2 <- function(x, y, ...) {
    if(xlog) x <- 10^x
    if(ylog) y <- 10^y
    points(x, y, ...)
  }
  text2 <- function(x, y, ...) {
    ##--- need to adjust  adj == c(xadj, yadj) ?? --
    if(xlog) x <- 10^x
    if(ylog) y <- 10^y
    text(x, y, ...)
  }

  cin <- par("cin")
  Cex <- cex * par("cex")  # = the 'effective' cex for text

  if(is.null(text.width))
    text.width <- max(strwidth(legend, u="user", cex=cex))
  else if(!is.numeric(text.width) || text.width < 0)
    stop("text.width must be numeric, >= 0")

  ## These defaults should  DEPEND  on  text.width (& maybe x/y log):
  if(is.null(x.intersp)) x.intersp <- min(4, 1.8 + 1.2* Cex^-1.25)
  if(is.null(y.intersp)) y.intersp <- min(2, 0.2 + Cex^-1.25)
  xc <- Cex * xinch(cin[1], warn.log=FALSE) # [uses par("usr") and "pin"]
  yc <- Cex * yinch(cin[2], warn.log=FALSE)

  xchar  <- xc
  yextra <- yc * (y.intersp - 1)
  ychar <- yextra + max(yc, strheight(legend, u="user", cex=cex))

  xbox <- xc * 0.8 ##= sizes of filled boxes.
  ybox <- yc * 0.8
  n.leg <- length(legend)
  ## -- (w,h) := (width,height) of the box to draw -- computed stepwise...
  w <- 2 * xchar + text.width
  h <- (n.leg + 1) * ychar
  if(!missing(fill))
    w <- w + (dx.fill <- xbox + xchar)
  if(!missing(pch)) {
    if(is.character(pch) && nchar(pch) > 1) {
      np <- nchar(pch)
      pch <- substr(rep(pch[1], np), 1:np, 1:np)
    }
    if(!merge) w <- w + (dx.pch <- x.intersp/2 * xchar)
  }
  do.lines <- (!missing(lty) && any(lty > 0)) || !missing(lwd)
  if(do.lines)
    if(!merge) w <- w + x.intersp * xchar

  if(merge) # we didn't add space above, so must do now
    w <- w + x.intersp * xchar
  ##
  ##-- (w,h) are now the final box width/height. --> Adjust (x,y) :

  if (xlog) x <- log10(x)
  if (ylog) y <- log10(y)
  if(length(x) != 1) { # in which situations do we need/want this ??
    x <- mean(x)
    y <- mean(y)
    xjust <- 0.5
    yjust <- 0.5
  }
  left <- x - xjust * w
  top  <- y + (1 - yjust) * h

  if (bty != "n")
    rect2(left, top, dx = w, dy = h, col = bg)

  ## (xt[],yt[]) := 'current' vectors of (x/y) legend text
  xt <- rep(left, n.leg) + xchar
  yt <- top - (1:n.leg) * ychar

  if (!missing(fill)) {                 #- draw filled boxes -------------
    rect2(xt, yt + ybox/2, dx = xbox, dy = ybox/2, col = fill)
    xt <- xt + dx.fill
  }
  col <- rep(col,length.out=n.leg)
  if (!missing(pch)) {                  #- draw points -------------------
    pch <- rep(pch,length.out=n.leg)
    ok <- is.character(pch) | pch > 0
    x1 <- (xt + ifelse(merge,0, 0.25) * xchar)[ok]
    y1 <- yt[ok]
    points2(x1, y1, pch=pch[ok], col=col[ok], cex=cex)
    if (!merge) xt <- xt + dx.pch
  }
  if (do.lines) {                       #- draw lines ---------------------
    if(missing(lty)) { lty <- 1; ok.l <- TRUE }
    else ok.l <- lty > 0
    if(missing(lwd)) lwd <- 1
    lty <- rep(lty, length.out = n.leg)
    lwd <- rep(lwd, length.out = n.leg)
    x.off <- if(merge) -0.8 else 0
    segments2(xt[ok.l] + x.off*xchar, yt[ok.l], dx= 2*xchar, dy=0,
              lty = lty[ok.l], #- next version of R: lwd = lwd[ok.l],
              col = col[ok.l])
    if (!merge) xt <- xt + 3 * xchar
  }
  if (merge) xt <- xt + x.intersp * xchar

  ## adj = (x,y) text-box adjustment
  text2(xt, yt, labels= legend, adj= c(0, 0.3*y.intersp), cex= cex)
}
