is.mts <- function (x) inherits(x, "mts")



ts.plot <- function(..., gpars=list())
{
    sers <- ts.union(...)
    if(is.null(gpars$ylab))
        gpars$ylab <- if(NCOL(sers) > 1) "" else deparse(substitute(...))
    do.call("plot.ts", c(list(sers, plot.type="single"), gpars))
}

Ops.ts <- function(e1, e2)
{
    if(missing(e2)) {
        ## univariate operator
        NextMethod(.Generic)
    } else if(any(nchar(.Method) == 0)) {
        ## one operand is not a ts
        NextMethod(.Generic)
    } else {
        ## use ts.intersect to align e1 and e2
        nc1 <- NCOL(e1)
        nc2 <- NCOL(e2)
        e12 <- ts.intersect(e1, e2)
        e1 <- if(is.matrix(e1)) e12[, 1:nc1, drop = FALSE] else e12[, 1]
        e2 <- if(is.matrix(e2)) e12[, nc1 + (1:nc2), drop = FALSE]
        else e12[, nc1 + 1]
        NextMethod(.Generic)
    }
}
