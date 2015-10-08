require("splines")

## Bug report PR#16549 - 'bad value from splineDesign'
## Date: Wed, 30 Sep 2015 12:12:47 +0000
## https://bugs.r-project.org/bugzilla/show_bug.cgi?id=16549

## Reporter: roconnor@health.usf.edu

knots <- c(0,0,0,0,1,1,1,1)
m1 <- rbind(c(1, 0,0,0))
stopifnot(
    ## the first gave (0 0 0 0) instead of m1 :
    all.equal(splineDesign(knots, c(0,2), outer.ok = TRUE), rbind(m1, 0)),
    all.equal(splineDesign(knots, 0, outer.ok = TRUE), m1),
    all.equal(splineDesign(knots, 0), m1),
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
    dk <- diff(rk <- range(knots))
    d.x <- dk / (2*n.k)
    x <- seq(min(knots)-d.x, max(knots)+d.x, length.out = n)
    bb <- splineDesign(knots, x = x, ord = ord, outer.ok = TRUE)
    is.x.in <- knots[ord] <= x & x < knots[n.k-(ord-1)]
    ##                   ~~~~     ~~~  same as in splineDesign(*, outer.ok=TRUE)
    sumB <- rowSums(bb)
    if(any(iBad <- !is.finite(sumB))) {
	chkSum.Ok <<- FALSE ## for check
	cat("** _FIXME_ NON-finite values in sumB: ord = ", ord, "; |knots| =", n.k,"\n")
	cat("knots <- "); dput(knots)
        cat("non-finite at x = "); dput(x[iBad])
    } else {
	eps <- 2*.Machine$double.eps
	stopifnot(abs(1 - sumB[is.x.in]) <= 2*eps, 0 <= sumB+eps, sumB-eps <= 1)
	## TODO: now also check derivatives
    }
    invisible(bb)
}

## from ../man/splineDesign.Rd :
knots <- c(1,1.8,3:5,6.5,7,8.1,9.2,10)  # 10 => 10-4 = 6 Basis splines
chkSum(knots)
chkSum(knots, ord=3)
chkSum(knots, ord=2)
chkSum(knots, ord=1)

set.seed(17)

for(n in 1:1000) {
    if(n %% 50 == 0) cat(sprintf("n = %4d\n",n))
    kn <- sort.int(round(10* rnorm(4 + rpois(1, lambda=4))))
    for(oo in 1:4)
        chkSum(kn, ord = oo)
}

## One of the cases with NaN {when used  ( . <= x & x <= . )}:
bb <- chkSum(c(-14, -4, 3, 5, 6, 15, 15))
which(!is.finite(rowSums(bb)))
## now integer(0)

stopifnot(chkSum.Ok)

proc.time()

###----------------- sparse / dense   interpSpline() ---------------------------

## from  help(interpSpline) -- ../man/interpSpline.Rd
ispl <- interpSpline( women$height, women$weight)
isp. <- interpSpline( women$height, women$weight, sparse=TRUE)
stopifnot(all.equal(ispl, isp., tol = 1e-12)) # seen 1.65e-14

ipStime <- function(n) { # and using 'ispl'
    h <- seq(55, 75, length.out = n)
    w1k <- data.frame(height = h, weight = predict(ispl, h)$y)
    c.d <- system.time(is.d <- interpSpline(weight ~ height, w1k, sparse=FALSE))
    c.s <- system.time(is.s <- interpSpline(weight ~ height, w1k, sparse=TRUE ))
    stopifnot(all.equal(is.d, is.s, tol = 1e-7)) # seen 9.4e-10 (n=1000), 1.3e-7 (n=5000)
    list(d.time = c.d, s.time = c.s)
}

n.s <- 25 * round(2^seq(1,6, by=.5))
(ipL <- lapply(setNames(n.s, paste0("n=",n.s)), ipStime))
## sparse is *an order of magnitude* faster for n ~= 1000 but somewhat slower for n ~< 200:
sapply(ipL, function(ip) round(ip$d.time / ip$s.time, 1)[c(1,3)])
##           n=50 n=75 n=100 n=150 n=200 n=275 n=400 n=575 n=800 n=1125 n=1600 -- nb-mm4, i7-5600U
## user.self  0.5  0.5   0.5   0.5   0.7   2.5   4.3  12.3  33.7   70.5  116.1
## elapsed    0.5  0.3   0.5   0.7   1.0   2.5   4.3  13.0  26.2   57.4  117.3
