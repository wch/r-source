recordPlot <- function()
{
    if(dev.cur() == 1)
        stop("no current device to record from")
    res <- vector("list", 2)
    res[1] <- list(.Internal(getDL()))
    res[[2]] <- .Internal(getGPar())
    names(res) <- c("displaylist", "gpar")
    class(res) <- "recordedplot"
    res
}

replayPlot <- function(x)
{
    if(dev.cur() == 1)
        stop("no current device to replay to")
    if(class(x) != "recordedplot")
        stop("argument is not of class \"recordedplot\"")
    plot.new()
    .Internal(setGPar(x[[2]]))
    .Internal(playDL(x[[1]]))
}

print.recordedplot <- function(x)
{
    replayPlot(x)
    invisible(x)
}
