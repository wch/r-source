rgb <- function(red, green, blue, names=NULL)
.Internal(rgb(red, green, blue, names))

hsv <- function(h=1,s=1,v=1,gamma=1)
.Internal(hsv(h,s,v,gamma))

palette <- function(value)
{
	if(missing(value)) .Internal(palette(character()))
	else invisible(.Internal(palette(value)))
}

## A quick little ``rainbow'' function -- improved by MM
					# doc in	../man/palettes.Rd
rainbow <-
function (n, s = 1, v = 1, start = 0, end = max(1,n - 1)/n, gamma = 1)
{
 if ((n <- as.integer(n[1])) > 0) {
	if(start == end || any(c(start,end) < 0)|| any(c(start,end) > 1))
		stop("`start' and `end' must be distinct and in [0,1].")
	hsv(h = seq(start, ifelse(start > end, 1, 0) + end, length= n) %% 1,
	    s, v, gamma)
 } else character(0)
}

topo.colors <- function (n)
{
 if ((n <- as.integer(n[1])) > 0) {
	j <- n %/% 3
	k <- n %/% 3
	i <- n - j - k
	c(if(i > 0) hsv(h= seq(from = 43/60, to = 31/60, length = i)),
	  if(j > 0) hsv(h= seq(from = 23/60, to = 11/60, length = j)),
	  if(k > 0) hsv(h= seq(from = 10/60, to =  6/60, length = k),
			s= seq(from = 1,     to = 0.3,	 length = k), v = 1))
 } else character(0)
}

terrain.colors <- function (n)
{
 if ((n <- as.integer(n[1])) > 0) {
	j <- n %/% 3
	k <- n %/% 3
	i <- n - j - k
	c(hsv(23/60, 1, v = seq(0.6, 0.85, length = i)),
	  if(j > 0)
		hsv(h = seq(22/60, 10/60, length = j), s = 1,
		    v = seq(0.85 ,     1, length = j)),
	  if(k > 0)
		hsv(h = seq(from = 9/60, to = 6/60, length = k),
		    s = seq(from =    1, to = 0.3,  length = k), v = 1))
 } else character(0)
}

heat.colors <- function (n)
{
 if ((n <- as.integer(n[1])) > 0) {
	j <- n %/% 4
	i <- n - j
	c(rainbow(i, start = 0, end = 1/6),
	  if (j > 0)
		hsv(h = 1/6, s = seq(from= 1-1/(2*j), to= 1/(2*j), length = j),
		    v = 1))
 } else character(0)
}
