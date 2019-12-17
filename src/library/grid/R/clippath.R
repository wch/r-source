
createClipPath <- function(clip) {
    force(clip)
    pathFun <- function() {
        grid.draw(clip)
    }
    path <- list(f=pathFun, index=-1)
    class(path) <- "GridClipPath"
    path
}   

## "resolve" clipping paths
resolveClipPath <- function(path) {
    index <- grDevices::setClipPath(path$f, as.integer(path$index))
    resolvedClipPath(path, index)
}

resolvedClipPath <- function(path, index) {
    UseMethod("resolvedClipPath")
}

resolvedClipPath.GridClipPath <- function(path, index) {
    index <- as.integer(index)
    path$index <- index
    class(path) <- c("GridResolvedClipPath", class(path))
    path
}    

resolvedClipPath.GridResolvedClipPath <- function(path, index) {
    index <- as.integer(index)
    path$index <- index
    path
}    

