#  File src/library/grDevices/R/colorstuff.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2012 The R Core Team
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

colors <- function() .Internal(colors())
colours <- colors
col2rgb <- function(col, alpha=FALSE) {
  result <- .Internal(col2rgb(col))
  if (!alpha)
    result <- result[1L:3,, drop=FALSE]
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
        structure(substr(result, 1L, 7L), names=names(result))
    else result
}

hsv <- function(h=1, s=1, v=1, alpha = 1)
{
    alphaspec <- !missing(alpha)
    result <- .Internal(hsv(h, s, v, alpha))
    ## If alpha not specified only return #RRGGBB
    if (!alphaspec)
        structure(substr(result, 1L, 7L), names=names(result))
    else result
}

hcl <-
function (h = 0, c = 35, l = 85, alpha = 1, fixup = TRUE)
{
    alphaspec <- !missing(alpha)
    result <- .Internal(hcl(h, c, l, alpha, fixup))
    if (!alphaspec)
        structure(substr(result, 1L, 7L), names=names(result))
    else result
}

rgb2hsv <- function(r, g = NULL, b = NULL, maxColorValue = 255)
{
    rgb <-
        if(is.null(g) && is.null(b)) as.matrix(r)
        else rbind(r,g,b)
    if(!is.numeric(rgb)) stop("rgb matrix must be numeric")
    d <- dim(rgb)
    if(d[1L] != 3) stop("rgb matrix must have 3 rows")
    n <- d[2L]
    if(n == 0L)
        return(cbind(c(h = 1, s = 1, v = 1))[, 0L])
    ## else:
    rgb <- rgb/maxColorValue
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
    function (n, s = 1, v = 1, start = 0, end = max(1,n - 1)/n, alpha = 1)
{
    if ((n <- as.integer(n[1L])) > 0) {
	if(start == end || any(c(start,end) < 0)|| any(c(start,end) > 1))
	    stop("'start' and 'end' must be distinct and in [0, 1].")
	hsv(h = seq.int(start, ifelse(start > end, 1, 0) + end,
                        length.out = n) %% 1, s, v, alpha)
    } else character()
}

topo.colors <- function (n, alpha = 1)
{
    if ((n <- as.integer(n[1L])) > 0) {
	j <- n %/% 3
	k <- n %/% 3
	i <- n - j - k
	c(if(i > 0) hsv(h= seq.int(from= 43/60, to= 31/60, length.out = i), alpha=alpha),
	  if(j > 0) hsv(h= seq.int(from= 23/60, to= 11/60, length.out = j), alpha=alpha),
	  if(k > 0) hsv(h= seq.int(from= 10/60, to=  6/60, length.out = k), alpha=alpha,
			s= seq.int(from= 1,	to= 0.3,   length.out = k), v = 1))
    } else character()
}

terrain.colors <- function (n, alpha = 1)
{
    if ((n <- as.integer(n[1L])) > 0) {
	k <- n%/%2
	h <- c(4/12, 2/12, 0/12)
	s <- c(1, 1, 0)
	v <- c(0.65, 0.9, 0.95)
	c(hsv(h = seq.int(h[1L], h[2L], length.out = k),
	      s = seq.int(s[1L], s[2L], length.out = k),
	      v = seq.int(v[1L], v[2L], length.out = k), alpha = alpha),
	  hsv(h = seq.int(h[2L], h[3L], length.out = n - k + 1)[-1L],
	      s = seq.int(s[2L], s[3L], length.out = n - k + 1)[-1L],
	      v = seq.int(v[2L], v[3L], length.out = n - k + 1)[-1L], alpha = alpha))
    } else character()
}

heat.colors <- function (n, alpha = 1)
{
    if ((n <- as.integer(n[1L])) > 0) {
	j <- n %/% 4
	i <- n - j
	c(rainbow(i, start = 0, end = 1/6, alpha = alpha),
	  if (j > 0)
	  hsv(h = 1/6, s = seq.int(from= 1-1/(2*j), to= 1/(2*j), length.out = j),
	      v = 1, alpha = alpha))
    } else character()
}

cm.colors <- function (n, alpha = 1)
{
    if ((n <- as.integer(n[1L])) > 0L) {
	even.n <- n %% 2L == 0L
	k <- n %/% 2L
	l1 <- k + 1L - even.n
	l2 <- n - k + even.n
	c(if(l1 > 0L)
	  hsv(h =  6/12, s = seq.int(.5, ifelse(even.n,.5/k,0), length.out = l1),
	      v = 1, alpha = alpha),
	  if(l2 > 1)
	  hsv(h = 10/12, s = seq.int(0, 0.5, length.out = l2)[-1L],
	      v = 1, alpha = alpha))
    } else character()
}

gray.colors <- ## FIXME: add 'alpha = 1'
function(n, start = 0.3, end = 0.9, gamma = 2.2)
    gray(seq.int(from = start^gamma, to = end^gamma, length.out = n)^(1/gamma))
grey.colors <- gray.colors
