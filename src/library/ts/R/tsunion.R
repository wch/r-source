ts.name.dots <- function(call, infer = TRUE)
{
    call <- call$...
    if(is.null(call))
        stop("No \"...\" argument in function definition")
    if(length(call) <= 1)
        character(0)
    else {
        nd <- names(call)
        if(infer) {
            na <- as.character(call)
            if(is.null(nd)) nd <- rep("", length(na))
            nd[nd == ""] <- na[nd == ""]
        }
        nd
    }
}

cbind.ts <- function(..., dframe = FALSE, union=TRUE)
{
    sers <- list(...)
    nser <- length(sers)
    if(nser == 0) return(NULL)
    if(nser == 1) return(sers[1])
    nsers <- sapply(sers, NCOL)
    nmsers <- ts.name.dots(match.call(expand.dots = FALSE))
    tsps <- sapply(list(...), tsp)
    freq <- round(mean(tsps[3,]))
    if(max(abs(tsps[3,] - freq)) > .Options$ts.eps) {
        stop("Not all series have the same frequency")
    }
    if(union) {
        st <- min(tsps[1, ])
        en <- max(tsps[2, ])
    } else {
        st <- max(tsps[1, ])
        en <- min(tsps[2, ])
        if(st > en) {
            warn("Non-intersecting series")
            return(NULL)
        }
    }
    p <- c(st, en, freq)
    n <- round(freq * (en - st) + 1)
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
ts.intersect <- function(...)
{
    m <- match.call()
    m[[1]] <- as.name("cbind.ts")
    m$union <- FALSE
    return(eval(m, sys.frame(sys.parent())))
}

