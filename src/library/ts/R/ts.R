ts.plot <- function(..., gpars=list())
{
    sers <- ts.union(...)
    if(is.null(gpars$ylab))
        gpars$ylab <- if(NCOL(sers) > 1) "" else deparse(substitute(...))
    do.call("plot.ts", c(list(sers, plot.type="single"), gpars))
}
