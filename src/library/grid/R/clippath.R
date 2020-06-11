
createClipPath <- function(clip) {
    force(clip)
    pathFun <- function() {
        grid.draw(clip, recording=FALSE)
    }
    path <- list(f=pathFun, ref=NULL)
    class(path) <- "GridClipPath"
    path
}   

isClipPath <- function(x) {
    inherits(x, "GridClipPath")
}

## "resolve" clipping paths
resolveClipPath <- function(path) {
    ref <- .setClipPath(path$f, path$ref)
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

unresolveClipPath <- function(path) {
    UseMethod("unresolveClipPath")
}
    
## Unresolved clipPaths just pass through
unresolveClipPath.GridClipPath <- function(path) {
    path
}

unresolveClipPath.GridResolvedClipPath <- function(path) {
    path <- list(f=path$f, ref=NULL)
    class(path) <- "GridClipPath"
    path
}

