lag <- function(x, ...) UseMethod("lag")

lag.default <- function(x, k = 1)
{
    if(k != round(k)) {
        k <- round(k)
        warning("k is not an integer")
    }
    x <- as.ts(x)
    p <- tsp(x)
    tsp(x) <- p - (k/p[3]) * c(1, 1, 0)
    x
}
cycle <- function(x, ...) UseMethod("cycle")

cycle.default <- function(x)
{
    p <- tsp(as.ts(x))
    m <- floor((p[1] %% 1) * p[3])
    x <- (1:NROW(x) + m - 1) %% p[3] + 1
    tsp(x) <- p
    x
}
deltat <- function(x, ...) UseMethod("deltat")
deltat.default <- function(x) 1/tsp(as.ts(x))[3]


na.omit.ts <- function(frame)
{
     if(is.matrix(frame))
         good <- which(apply(!is.na(frame), 1, all))
     else  good <- which(!is.na(frame))
     if(!length(good)) stop("all times contain an NA")
     omit <- integer(0)
     n <- NROW(frame)
     st <- min(good)
     if(st > 1) omit <- c(omit, 1:(st-1))
     en <- max(good)
     if(en < n) omit <- c(omit, (en+1):n)
     if(length(omit)) {
         frame <- if(is.matrix(frame)) frame[st:en,] else frame[st:en]
        temp <- seq(omit)[omit]
        names(temp) <- time(frame)[omit]
        attr(temp, "class") <- "omit"
        attr(frame, "na.action") <- temp
     }
     if(any(is.na(frame))) stop("time series contains internal NAs")
     frame
}
