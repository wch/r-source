na.contiguous <- function(frame)
{
    tm <- time(frame)
    xfreq <- frequency(frame)
    ## use (first) maximal contiguous length of non-NAs
    if(is.matrix(frame))
        good <- apply(!is.na(frame), 1, all)
    else  good <- !is.na(frame)
    if(!sum(good)) stop("all times contain an NA")
    tt <- cumsum(!good)
    ln <- sapply(0:max(tt), function(i) sum(tt==i))
    seg <- (seq(along=ln)[ln==max(ln)])[1] - 1
    keep <- (tt == seg)
    st <- min(which(keep))
    if(!good[st]) st <- st + 1
    en <- max(which(keep))
    omit <- integer(0)
    n <- NROW(frame)
    if(st > 1) omit <- c(omit, 1:(st-1))
    if(en < n) omit <- c(omit, (en+1):n)
    cl <- class(frame)
    if(length(omit)) {
        frame <- if(is.matrix(frame)) frame[st:en,] else frame[st:en]
        attr(omit, "class") <- "omit"
        attr(frame, "na.action") <- omit
        tsp(frame) <- c(tm[st], tm[en], xfreq)
        if(!is.null(cl)) class(frame) <- cl
    }
    frame
}
