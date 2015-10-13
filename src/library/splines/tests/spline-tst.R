require("splines")

## Bug report PR#16549 - 'bad value from splineDesign'
## Date: Wed, 30 Sep 2015 12:12:47 +0000
## https://bugs.r-project.org/bugzilla/show_bug.cgi?id=16549

## Reporter: roconnor@health.usf.edu  (extended original example code)

kn8.01 <- c(0,0,0,0,1,1,1,1)
m1  <- rbind(c(1, 0,0,0))
m.1 <- rbind(c(0,0,0, 1))
stopifnot(
    ## the first gave (0 0 0 0) instead of m1 :
    all.equal(splineDesign(kn8.01, c(0,2),    outer.ok = TRUE), rbind(m1, 0)),
    all.equal(splineDesign(kn8.01, c( 0,1,2), outer.ok = TRUE), rbind(m1, m.1, 0)),
    all.equal(splineDesign(kn8.01, c(-1,0,1), outer.ok = TRUE), rbind(0, m1, m.1)),
    all.equal(splineDesign(kn8.01, 0, outer.ok = TRUE), m1),
    all.equal(splineDesign(kn8.01, 0), m1),
    TRUE)

## The original fix proposal introduced a new bug, visible here:
S <- splineDesign(c(-3, -3, -2, 0, 2, 3, 3), x= -3:3, outer.ok=TRUE)
## (had a NaN in the lower-right corner)
stopifnot(all.equal(S,
		    rbind(0, c(22,3,0)/45, c(193, 6*23, 9)/360,
			  c(1,3,1)/5,
			  c(9, 6*23, 193)/360,
			  c(0,3,22)/45, 0),
		    tolerance = 1e-14))

chkSum.Ok <- TRUE ## for check
chkSum <- function(knots, n = 1 + 2^9, ord = 4) {
    stopifnot(is.numeric(knots), !is.unsorted(knots), (n.k <- length(knots)) >= ord)
    dk <- diff(rk <- range(uk <- unique(knots)))
    if(dk == 0) dk <- uk[1]/4
    d.x <- dk / (2*n.k)
    ## x: the unique knots
    x <- sort(c(uk, seq.int(min(knots)-d.x, max(knots)+d.x, length.out = n)))
    bb <- splineDesign(knots, x = x, ord = ord, outer.ok = TRUE)
    is.x.in <- knots[ord] <= x & x <= knots[n.k-(ord-1)]
    ##                   ~~~~     ~~~~  same as in splineDesign(*, outer.ok=TRUE)
    ##
### no longer needed:
    ## work around "infelicity" (or not; not sure if to call "bug")
    ## n.RHk := #{duplicated RHS boundary knots}
    ## n.RHk <- match(FALSE, rev(duplicated(knots)))
    ## if(ord > 1 && n.RHk > ord) { ## the (ord == 1) case "works" via spike = 1
    ##     ## <==> knots[n.k -j ] == knots[n.k] for j = 0,1,..,(n.RHk-1)
    ##     ## then spl(x= RHS-knot, knots) == 0, even though "should" be 1
    ##     ## "FIX it up" for here. FIXME: do in splineDesign() or it's C code ?!
    ##     iR <- which(x == knots[n.k])
    ##     ## ncol(bb) == n.k - ord
    ##     j <- n.k - n.RHk # == ncol(bb) - (n.RHk - ord)
    ##     if(any(bb[iR, j] == 0)) bb[iR, j] <- 1
    ## }
    sumB <- rowSums(bb)
    if(any(iBad <- !is.finite(sumB))) {
	chkSum.Ok <<- FALSE ## for check
	cat("** _FIXME_ NON-finite values in sumB: ord = ", ord, "; |knots| =", n.k,"\n")
	cat("knots <- "); dput(knots)
	cat("non-finite at x = "); dput(x[iBad])
    } else if(length(bb)) { # only when bb[] is not 0-dimensional
	eps <- 3*.Machine$double.eps
	stopifnot(abs(1 - sumB[is.x.in]) <= 2*eps, 0 <= sumB+eps, sumB-2*eps <= 1)
	## TODO: now also check derivatives
    }
    invisible(bb)
}

plotSplD <- function(knots, n = 2^10, ord = 4, type = "l",
		     ylim = c(0,1), ylab = "B-splines", ...) {
    stopifnot(is.numeric(knots), !is.unsorted(knots), (n.k <- length(knots)) >= ord)
    dk <- diff(range(uk <- unique(knots)))
    if(dk == 0) dk <- uk[1]/4
    d.x <- dk / (2*n.k)
    ## x: will contain the unique knots uk
    x <- sort(c(uk, seq.int(min(knots)-d.x, max(knots)+d.x, length.out = n)))
    bb <- splineDesign(knots, x = x, ord = ord, outer.ok = TRUE)
    matplot(x, bb, type=type, ylim=ylim, ylab=ylab, ...)
    sumB <- rowSums(bb)
    abline(v = knots, lty = 3, col = "light gray")
    abline(v = knots[c(ord, n.k-(ord-1))], lty = 3, col = "gray10")
    lines(x, sumB, col = adjustcolor("red", 0.4), lwd = 3)
    abline(h=1, lty=2, col = adjustcolor(1, 0.4), lwd = 2)
    ## lty, col,: from matplot()
    legend(mean(x), 0.98, legend = paste0("B_", 1:ncol(bb)), lty=1:5, col=1:6,
	   bty = "n", xjust = 0.5)
    invisible(list(x=x, splineDesign = bb))
}

if(!dev.interactive(orNone=TRUE)) pdf("spline-tst.pdf")

plotSplD(kn8.01)
chkSum  (kn8.01)

## from ../man/splineDesign.Rd :
knots <- c(1,1.8,3:5,6.5,7,8.1,9.2,10)  # 10 => 10-4 = 6 Basis splines
str(plotSplD(knots))       # cubic     splines, adding to 1 in [ 4, 7 ]
str(plotSplD(knots, ord=3))# quadratic splines, adding to 1 in [ 3, 8.1]
str(plotSplD(knots, ord=2))# linear    splines, adding to 1 in [1.8,9.2]
str(plotSplD(knots, ord=1))# constant  splines, adding to 1 in [1, 10]

chkSum(knots)
chkSum(knots, ord=3)
chkSum(knots, ord=2)
chkSum(knots, ord=1)

## cases that failed too {Linux lynne 4.1.6 x86_64}
chkSum(c(1:5, 9,9),     ord=2) # ok for ord in {1,3,4}
chkSum(c(1:4, 9,9,9),   ord=3)
chkSum(c(1:3, 9,9,9,9), ord=4)
## These failed in R <= 3.2.2 (but not after first fix):
chkSum(c(0,0,0,0, 1:3), ord=4)
chkSum(c(0,0,0,   1:4), ord=3)
chkSum(c(0,0,     1:5), ord=2)

## This should be symmetric, but was not;
## the graphic is maybe most convincing:
k6.01 <- c(0,0,0, 1,1,1)
round(with(plotSplD(k6.01, ord=2, n=16), cbind(x, splineDesign)), 4)
x8 <- (0:8)/8
sp8 <- splineDesign(k6.01, x=x8, ord=2)
print.table(8*sp8, zero.print=".")
stopifnot(all.equal(8*sp8,
		    cbind(0, 8:0, 0:8, 0), tol = 1e-14))
## or just
splineDesign(k6.01, x=0:1, ord=2)
##  0 1 0 0
##  0 0 1 0 --- [finally !]
stopifnot(identical(cbind(0, diag(2), 0),
		    splineDesign(k6.01, x=0:1, ord=2)))

## Further:
for(k in 0:8)
    chkSum(c(0:k, 9,9,9),     ord=2)
for(k in 0:8)
    chkSum(c(0:k, 9,9,9,9),   ord=3)
for(k in 0:8)
    chkSum(c(0:k, 9,9,9,9,9), ord=4)

## look at two examples
r  <- plotSplD(c(0:4, 8.9,9,9,9), ord=3)# B_6 is narrow spike
r. <- plotSplD(c(0:4,  9, 9,9,9), ord=3)# B_6 diverged to all zero.
if(interactive())
    round(with(r., cbind(x, splineDesign)), 4)
stopifnot(r.$splineDesign[, 6] == 0)
## one could argue that B_6 should also have finite area, and hence be "a delta".
## ==> sum_j B_j(x = 9) would then be Inf or undefined ..
## more sensible: B_6 should be omitted and should have B_5(9) == 1
## now implemented:
stopifnot(identical(c(0, 0, 0, 0, 1, 0),
		    with(r., splineDesign[x == 9,])))

## Everything is fine, if  left-right  mirrored / reversed :
r03 <- plotSplD(c(0,0,0,0,  1:5), ord=3)
## here, B_1 is "delta" and should rather be omitted
stopifnot(identical(c(0, 1, 0, 0, 0, 0),
		    with(r03, splineDesign[x == 0,])))
## 0 1 0 0 0 0 -- as it should be ..
round(with(r03, cbind(x, splineDesign)[50:60,]), 4)

r04 <- plotSplD(c(0,0,0,0,  1:5), ord=4)
## here, B_1 is "delta" and should rather be omitted
stopifnot(identical(c(1, 0, 0, 0, 0),
		    with(r04, splineDesign[x == 0,])))
round(with(r04, cbind(x, splineDesign)[50:60,]), 4)

r0.4 <- plotSplD(c(0,0,0, 1:5), ord=4) # basis is nice and correct


set.seed(17)

for(n in 1:1000) {
    if(n %% 50 == 0) cat(sprintf("n = %4d\n",n))
    kn <- sort.int(round(10* rnorm(4 + rpois(1, lambda=4))))
    for(oo in 1:4)
	## if(inherits(r <- tryCatch(chkSum(kn, ord=oo), error=identity), "error"))
	##     cat(r$message, "\n chkSum(", deparse(kn), ", ord = ", oo, ")\n")
	chkSum(kn, ord = oo)
}

## One of the cases with NaN {when used  ( . <= x & x <= . )}:

bb <- chkSum(c(-14, -4, 3, 5, 6, 15, 15))
stopifnot(is.finite(rowSums(bb)))


stopifnot(chkSum.Ok)

proc.time()

###----------------- sparse / dense   interpSpline() ---------------------------

## from  help(interpSpline) -- ../man/interpSpline.Rd
ispl <- interpSpline( women$height, women$weight)
isp. <- interpSpline( women$height, women$weight, sparse=TRUE)
stopifnot(all.equal(ispl, isp., tol = 1e-12)) # seen 1.65e-14

##' @title Interpolate size-n version of the 'women' data sparsely and densely
##' @param n size of "women-like" data to interpolate
##' @return list with dense and sparse \code{\link{system.time}()}s
##' @author Martin Maechler
ipStime <- function(n) { # and using 'ispl'
    h <- seq(55, 75, length.out = n)
    w <- predict(ispl, h)$y
    c.d <- system.time(is.d <- interpSpline(h, w, sparse=FALSE))
    c.s <- system.time(is.s <- interpSpline(h, w, sparse=TRUE ))
    stopifnot(all.equal(is.d, is.s, tol = 1e-7)) # seen 9.4e-10 (n=1000), 1.3e-7 (n=5000)
    list(d.time = c.d, s.time = c.s)
}

n.s <- 25 * round(2^seq(1,6, by=.5))
if(!interactive())# save 'check time'
    n.s <- n.s[100 <= n.s & n.s <= 800]
(ipL <- lapply(setNames(n.s, paste0("n=",n.s)), ipStime))
## sparse is *an order of magnitude* faster for n ~= 1000 but somewhat slower for n ~< 200:
sapply(ipL, function(ip) round(ip$d.time / ip$s.time, 1)[c(1,3)])
##           n=50 n=75 n=100 n=150 n=200 n=275 n=400 n=575 n=800 n=1125 n=1600 -- nb-mm4, i7-5600U
## user.self  0.5  0.5   0.5   0.5   0.7   2.5   4.3  12.3  33.7   70.5  116.1
## elapsed    0.5  0.3   0.5   0.7   1.0   2.5   4.3  13.0  26.2   57.4  117.3
