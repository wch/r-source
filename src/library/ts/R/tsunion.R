cbind.ts <- function(..., dframe = FALSE, union = TRUE)
{
    names.dots <- function(...)
    {
        l <- as.list(substitute(list(...)))[-1]
        nm <- names(l)
        fixup <- if (is.null(nm)) seq(along = l) else nm == ""
        dep <- sapply(l[fixup], function(x) deparse(x)[1])
        if (is.null(nm)) dep
        else {
            nm[fixup] <- dep
            nm
        }
    }
    sers <- list(...)
    nulls <- sapply(sers, is.null)
    sers <- sers[!nulls]
    nser <- length(sers)
    if(nser == 0) return(NULL)
    if(nser == 1)
        if(dframe) return(as.data.frame(sers[[1]])) else return(sers[[1]])
    nmsers <- names.dots(...)
    tsser <-  sapply(sers, function(x) length(tsp(x)) > 0)
    if(!any(tsser))
        stop("no time series supplied")
    sers <- lapply(sers, as.ts)
    nsers <- sapply(sers, NCOL)
    tsps <- sapply(sers[tsser], tsp)
    freq <- mean(tsps[3,])
    if(max(abs(tsps[3,] - freq)) > .Options$ts.eps) {
        stop("Not all series have the same frequency")
    }
    if(union) {
        st <- min(tsps[1,])
        en <- max(tsps[2,])
    } else {
        st <- max(tsps[1,])
        en <- min(tsps[2,])
        if(st > en) {
            warning("Non-intersecting series")
            return(NULL)
        }
    }
    p <- c(st, en, freq)
    n <- round(freq * (en - st) + 1)
    if(any(!tsser)) {
        ln <- lapply(sers[!tsser], NROW)
        if(any(ln != 1 && ln != n))
            stop("non-time series not of the correct length")
        for(i in (1:nser)[!tsser]) {
            sers[[i]] <- ts(sers[[i]], start=st, end=en, frequency=freq)
        }
        tsps <- sapply(sers, tsp)
    }
    if(dframe) {
        x <- vector("list", n)
        names(x) <- nmsers
    } else {
        ns <- sum(nsers)
        x <- matrix(, n, ns)
        cs <- c(0, cumsum(nsers))
        nm <- character(ns)
        for(i in 1:nser)
            if(nsers[i] > 1) {
                cn <- colnames(sers[[i]])
                if(is.null(cn)) cn <- 1:nsers[i]
                nm[(1+cs[i]):cs[i+1]] <- paste(nmsers[i], cn, sep=".")
            } else nm[cs[i+1]] <- nmsers[i]
        dimnames(x) <- list(NULL, nm)
    }
    for(i in 1:nser) {
        if(union) {
            xx <-
                if(nsers[i] > 1)
                    rbind(matrix(NA, round(freq * (tsps[1,i] - st)), nsers[i]),
                          sers[[i]],
                          matrix(NA, round(freq * (en - tsps[2,i])), nsers[i]))
                else
                    c(rep(NA, round(freq * (tsps[1,i] - st))), sers[[i]],
                      rep(NA, round(freq * (en - tsps[2,i]))))
        } else {
            xx <- window(sers[[i]], st, en)
        }
        if(dframe) x[[i]] <- structure(xx, tsp=p, class="ts")
        else x[, (1+cs[i]):cs[i+1]] <- xx
    }
    if(dframe) as.data.frame(x)
    else ts(x, start=st, freq=freq)
}

ts.union <- .Alias(cbind.ts)
ts.intersect <- function(...) cbind.ts(..., union=FALSE)
