recordPlot <- function()
{
    if(dev.cur() == 1)
        stop("no current device to record from")
    res <- .Internal(getSnapshot())
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
    nm <- names(x)
    if(length(nm) == 2 && nm == c("displaylist", "gpar")) {
        ## pre-1.4.0 save
        .Internal(setGPar(x[[2]]))
        .Internal(playDL(x[[1]]))
    } else .Internal(playSnapshot(x))
}

print.recordedplot <- function(x, ...)
{
    replayPlot(x)
    invisible(x)
}

