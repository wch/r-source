filter <- function(x, filter, method = c("convolution", "recursive"),
                   sides = 2, circular = FALSE, init=NULL)
{
    method <- match.arg(method)
    x <- as.ts(x)
    xtsp <- tsp(x)
    x <- as.matrix(x)
    n <- nrow(x)
    nser <- ncol(x)
    nfilt <- length(filter)
    if(any(is.na(filter))) stop("missing values in 'filter'")
    y <- matrix(NA, n, nser)
    if(method == "convolution") {
        if(nfilt > n) stop("'filter' is longer than time series")
        if(sides != 1 && sides != 2)
            stop("argument sides must be 1 or 2")
        for (i in 1:nser)
            y[, i] <- .C("filter1",
                         as.double(x[,i]),
                         as.integer(n),
                         as.double(filter),
                         as.integer(nfilt),
                         as.integer(sides),
                         as.integer(circular),
                         out=double(n), NAOK=TRUE,
                         PACKAGE = "stats")$out
    } else {
        if(missing(init)) {
            init <- matrix(0, nfilt, nser)
        } else {
            ni <- NROW(init)
            if(ni != nfilt)
                stop("length of 'init' must equal length of 'filter'")
            if(NCOL(init) != 1 && NCOL(init) != nser)
                stop(gettextf("'init'; must have 1 or %d cols", nser),
                     domain = NA)
            if(!is.matrix(init)) init <- matrix(init, nfilt, nser)
        }
        for (i in 1:nser)
            y[, i] <- .C("filter2",
                         as.double(x[,i]),
                         as.integer(n),
                         as.double(filter),
                         as.integer(nfilt),
                         out=as.double(c(rev(init[, i]), double(n))),
                         NAOK=TRUE,
                         PACKAGE = "stats")$out[-(1:nfilt)]
    }
    y <- drop(y)
    tsp(y) <- xtsp
    class(y) <- if(nser > 1) c("mts", "ts") else "ts"
    y
}

