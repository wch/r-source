na.contiguous <- function(object, ...) UseMethod("na.contiguous")

na.contiguous.default <- function(object, ...)
{
    tm <- time(object)
    xfreq <- frequency(object)
    ## use (first) maximal contiguous length of non-NAs
    if(is.matrix(object))
        good <- apply(!is.na(object), 1, all)
    else  good <- !is.na(object)
    if(!sum(good)) stop("all times contain an NA")
    tt <- cumsum(!good)
    ln <- sapply(0:max(tt), function(i) sum(tt==i))
    seg <- (seq_along(ln)[ln==max(ln)])[1] - 1
    keep <- (tt == seg)
    st <- min(which(keep))
    if(!good[st]) st <- st + 1
    en <- max(which(keep))
    omit <- integer(0)
    n <- NROW(object)
    if(st > 1) omit <- c(omit, 1:(st-1))
    if(en < n) omit <- c(omit, (en+1):n)
    cl <- class(object)
    if(length(omit)) {
        object <- if(is.matrix(object)) object[st:en,] else object[st:en]
        attr(omit, "class") <- "omit"
        attr(object, "na.action") <- omit
        tsp(object) <- c(tm[st], tm[en], xfreq)
        if(!is.null(cl)) class(object) <- cl
    }
    object
}
