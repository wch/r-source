ts.plot <- function(..., gpars = list())
{
    dots <- list(...)
    pars <- c("xlab", "ylab", "xlim", "ylim", "col", "lty", "lwd",
              "type", "main", "sub", "log")
    m <- names(dots) %in% pars
    if(length(m)) {
        gpars <- c(gpars, dots[m])
        dots <- dots[!m]
    }
    sers <- do.call("ts.union", dots)
    if(is.null(gpars$ylab))
        gpars$ylab <- if(NCOL(sers) > 1) "" else deparse(substitute(...))
    do.call("plot.ts", c(list(sers, plot.type = "single"), gpars))
}

arima.sim <- function(model, n, rand.gen = rnorm,
                      innov = rand.gen(n, ...), n.start = NA, ...)
{
    if(!is.list(model)) stop("`model' must be list")
    nm <- names(model)
    p <- length(model$ar)
    if(p) {
        minroots <- min(Mod(polyroot(c(1, -model$ar))))
        if(minroots <= 1) stop("ar part of model is not stationary")
    }
    q <- length(model$ma)
    if(is.na(n.start)) n.start <- p + q +
        ifelse(p > 0, ceiling(6/log(minroots)), 0)
    if(n.start < p + q) stop("burn-in must be as long as ar + ma")
    d <- 0
    if(!is.null(ord <- model$order)) {
        if(length(ord) != 3) stop("`model$order' must be of length 3")
        d <- ord[2]
        if(d != round(d) || d < 0)
            stop("number of differences must be a positive integer")
    }
    x <- ts(c(rand.gen(n.start, ...), innov[1:n]), start = 1 - n.start)
    if(length(model$ma)) x <- filter(x, c(1, model$ma), sides = 1)
    if(length(model$ar)) x <- filter(x, model$ar, method = "recursive")
    x <- x[-(1:n.start)]
    if(d > 0) x <- diffinv(x, differences = d)
    else as.ts(x)
}
