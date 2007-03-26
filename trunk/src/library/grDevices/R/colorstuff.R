colors <- function() .Internal(colors())
colours <- colors
col2rgb <- function(col, alpha=FALSE) {
  result <- .Internal(col2rgb(col))
  if (!alpha)
    result <- result[1:3,, drop=FALSE]
  result
}

## FIXME: gray() should also allow alpha; ditto for gray.colors() below
gray <- function(level) .Internal(gray(level))
grey <- gray

rgb <- function(red, green, blue, alpha, names = NULL, maxColorValue = 1)
{
    alphaspec <- !missing(alpha)
    if(!alphaspec)
	alpha <- maxColorValue
	
    ## Only red
    if(missing(green) && missing(blue)) {
	if(is.matrix(red) || is.data.frame(red)) {
	    red <- data.matrix(red)
	    if(ncol(red) < 3) stop("at least 3 columns needed")
	    green <- red[,2]
	    blue <- red[,3]
	    red <- red[,1]
	}
    }

    ## in the first case, (r,g,b) are (coerced to) integer, otherwise
    ## double :
    if(maxColorValue == 255)
        result <- .Internal(rgb256(red, green, blue, alpha, names))
    else
        result <- .Internal(rgb(red, green, blue, alpha, maxColorValue, names))
    ## If alpha not specified only return #RRGGBB
    if (!alphaspec)
        structure(substr(result, 1, 7), names=names(result))
    else result
}

hsv <- function(h=1, s=1, v=1, gamma = 1, alpha = 1)
{
    alphaspec <- !missing(alpha)
    result <- .Internal(hsv(h, s, v, gamma, alpha))
    ## If alpha not specified only return #RRGGBB
    if (!alphaspec)
        structure(substr(result, 1, 7), names=names(result))
    else result
}

hcl <-
function (h = 0, c = 35, l = 85, alpha = 1, fixup = TRUE)
{
    alphaspec <- !missing(alpha)
    result <- .Internal(hcl(h, c, l, alpha, fixup))
    if (!alphaspec)
        structure(substr(result, 1, 7), names=names(result))
    else result
}

rgb2hsv <- function(r, g = NULL, b = NULL, gamma = 1, maxColorValue = 255)
{
    rgb <-
        if(is.null(g) && is.null(b)) as.matrix(r)
        else rbind(r,g,b)
    if(!is.numeric(rgb)) stop("rgb matrix must be numeric")
    d <- dim(rgb)
    if(d[1] != 3) stop("rgb matrix must have 3 rows")
    n <- d[2]
    if(n == 0)
        return(cbind(c(h=1,s=1,v=1))[,0])
    ## else:
    rgb <- rgb/maxColorValue
    if(gamma != 1)# revert gamma corrected hsv values
        rgb <- rgb ^ (1/gamma)
    if(any(0 > rgb) || any(rgb > 1))
        stop("rgb values must be in [0, maxColorValue]")

    .Internal(rgb2hsv(rgb))
}

palette <- function(value)
{
    if(missing(value)) .Internal(palette(character()))
    else invisible(.Internal(palette(value)))
}

## A quick little ''rainbow'' function -- improved by MM
## doc in	../man/palettes.Rd
rainbow <-
    function (n, s = 1, v = 1, start = 0, end = max(1,n - 1)/n, gamma = 1, alpha = 1)
{
    if ((n <- as.integer(n[1])) > 0) {
	if(start == end || any(c(start,end) < 0)|| any(c(start,end) > 1))
	    stop("'start' and 'end' must be distinct and in [0, 1].")
	hsv(h = seq.int(start, ifelse(start > end, 1, 0) + end, length= n) %% 1,
	    s, v, gamma, alpha)
    } else character(0)
}

topo.colors <- function (n, alpha = 1)
{
    if ((n <- as.integer(n[1])) > 0) {
	j <- n %/% 3
	k <- n %/% 3
	i <- n - j - k
	c(if(i > 0) hsv(h= seq.int(from= 43/60, to= 31/60, length = i), alpha=alpha),
	  if(j > 0) hsv(h= seq.int(from= 23/60, to= 11/60, length = j), alpha=alpha),
	  if(k > 0) hsv(h= seq.int(from= 10/60, to=  6/60, length = k), alpha=alpha,
			s= seq.int(from= 1,	to= 0.3,   length = k), v = 1))
    } else character(0)
}

terrain.colors <- function (n, alpha = 1)
{
    if ((n <- as.integer(n[1])) > 0) {
	k <- n%/%2
	h <- c(4/12, 2/12, 0/12)
	s <- c(1, 1, 0)
	v <- c(0.65, 0.9, 0.95)
	c(hsv(h = seq.int(h[1], h[2], length = k),
	      s = seq.int(s[1], s[2], length = k),
	      v = seq.int(v[1], v[2], length = k), alpha = alpha),
	  hsv(h = seq.int(h[2], h[3], length = n - k + 1)[-1],
	      s = seq.int(s[2], s[3], length = n - k + 1)[-1],
	      v = seq.int(v[2], v[3], length = n - k + 1)[-1], alpha = alpha))
    } else character(0)
}

heat.colors <- function (n, alpha = 1)
{
    if ((n <- as.integer(n[1])) > 0) {
	j <- n %/% 4
	i <- n - j
	c(rainbow(i, start = 0, end = 1/6, alpha = alpha),
	  if (j > 0)
	  hsv(h = 1/6, s = seq.int(from= 1-1/(2*j), to= 1/(2*j), length = j),
	      v = 1, alpha = alpha))
    } else character(0)
}

cm.colors <- function (n, alpha = 1)
{
    if ((n <- as.integer(n[1])) > 0) {
	even.n <- n %% 2 == 0
	k <- n%/%2
	l1 <- k + 1 - even.n
	l2 <- n - k + even.n
	c(if(l1 > 0)
	  hsv(h =  6/12, s = seq.int(.5, ifelse(even.n,.5/k,0), length = l1),
	      v = 1, alpha = alpha),
	  if(l2 > 1)
	  hsv(h = 10/12, s = seq.int(0, 0.5, length = l2)[-1],
	      v = 1, alpha = alpha))
    } else character(0)
}

gray.colors <- ## FIXME: add 'alpha = 1'
function(n, start = 0.3, end = 0.9, gamma = 2.2)
    gray(seq.int(from = start^gamma, to = end^gamma, length = n)^(1/gamma))
grey.colors <- gray.colors
