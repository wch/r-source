quantile <- function(x, ...) UseMethod("quantile")

quantile.default <-
    function(x, probs = seq(0, 1, 0.25), na.rm = FALSE, names = TRUE)
{
    if (na.rm)
	x <- x[!is.na(x)]
    else if (any(is.na(x)))
	stop("Missing values and NaN's not allowed if `na.rm' is FALSE")
    if (any((p.ok <- !is.na(probs)) & (probs < 0 | probs > 1)))
	stop("probs outside [0,1]")
    n <- length(x)
    if(na.p <- any(!p.ok)) { # set aside NA & NaN
        o.pr <- probs
        probs <- probs[p.ok]
    }
    np <- length(probs)
    if(n > 0 && np > 0) {
	index <- 1 + (n - 1) * probs
	lo <- floor(index)
	hi <- ceiling(index)
	x <- sort(x, partial = unique(c(lo, hi)))
	i <- index > lo
	qs <- x[lo]
        i <- seq(along=i)[i & !is.na(i)][qs[i] > -Inf]
        .minus <- function(x,y) ifelse(x == y, 0, x - y)# ok for Inf - Inf
        qs[i] <- qs[i] + .minus(x[hi[i]], x[lo[i]]) * (index[i] - lo[i])
    }
    else {
	qs <- rep(as.numeric(NA), np)
    }
    if(names && np > 0) {
	dig <- max(2, getOption("digits"))
	names(qs) <- paste(## formatC is slow for long probs
			   if(np < 100)
			   formatC(100*probs, format="fg", wid = 1, dig=dig)
			   else format(100 * probs, trim=TRUE, dig=dig),
			   "%", sep = "")
    }
    if(na.p) { # do this more elegantly (?!)
        o.pr[p.ok] <- qs
        names(o.pr)[p.ok] <- names(qs)
        o.pr
    } else qs
}

IQR <- function (x, na.rm = FALSE)
    diff(quantile(as.numeric(x), c(0.25, 0.75), na.rm = na.rm, names = FALSE))
