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


