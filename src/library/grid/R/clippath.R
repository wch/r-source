
createClipPath <- function(clip) {
    force(clip)
    pathFun <- function() {
        grid.draw(clip, recording=FALSE)
    }
    path <- list(f=pathFun, ref=NULL)
    class(path) <- "GridClipPath"
    path
}   

## "resolve" clipping paths
resolveClipPath <- function(path) {
    UseMethod("resolveClipPath")
}

## A "placeholder" clipping path that is used during
## resolution of a clipping path to handle nested clipping paths
resolveClipPath.GridResolvingClipPath <- function(path) {
    ## Do nothing
}

resolveClipPath.GridClipPath <- function(path) {
    ref <- grDevices::setClipPath(path$f, path$ref)
    resolvedClipPath(path, ref)
}

resolvedClipPath <- function(path, ref) {
    UseMethod("resolvedClipPath")
}

resolvedClipPath.GridClipPath <- function(path, ref) {
    path$ref <- ref
    class(path) <- c("GridResolvedClipPath", class(path))
    path
}    

resolvedClipPath.GridResolvedClipPath <- function(path, ref) {
    path$ref <- ref
    path
}    

