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

arima.sim <- function(n, model, rand.gen = rnorm,
                      innov = rand.gen(n, ...), n.start = NA, ...)
{
    minroots <- min(Mod(polyroot(c(1, -model$ar))))
    if (minroots <= 1) stop("ar part of model is not stationary")
    p <- length(model$ar)
    q <- length(model$ma)
    if(is.na(n.start)) n.start <- p + q + ceiling(6/log(minroots))
    if(n.start < p + q) stop("burn-in must be as long as ar + ma")
    x <- ts(c(rnorm(n.start), innov[1:n]), start = 1 - n.start)
    if(length(model$ma)) x <- filter(x, c(1, model$ma), sides = 1)
    if(length(model$ar)) x <- filter(x, model$ar, method = "recursive")
    as.ts(x[-(1:n.start)])
}
