rversion <- function() {
  paste(R.Version()[c("major", "minor")],
                                  collapse=".")
}

recordPlot <- function()
{
    if(dev.cur() == 1)
        stop("no current device to record from")
    res <- .Internal(getSnapshot())
    attr(res, "version") <- rversion()
    class(res) <- "recordedplot"
    res
}

replayPlot <- function(x)
{
    if(class(x) != "recordedplot")
        stop("argument is not of class \"recordedplot\"")
    nm <- names(x)
    if(length(nm) == 2 && nm == c("displaylist", "gpar")) {
        ## pre-1.4.0 save
        .Internal(setGPar(x[[2]]))
        .Internal(playDL(x[[1]]))
    } else {
      version <- attr(x, "version")
      if (is.null(version))
        warning("loading snapshot from pre-2.0.0 R version")
      else if (version != rversion())
        warning("loading snapshot from different R version (", version, ")")
      .Internal(playSnapshot(x))
    }
}

print.recordedplot <- function(x, ...)
{
    replayPlot(x)
    invisible(x)
}

