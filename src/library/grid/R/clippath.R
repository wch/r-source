
createClipPath <- function(clip) {
    force(clip)
    pathFun <- function() {
        grid.draw(clip)
    }
    path <- list(f=pathFun)
    class(path) <- "GridClipPath"
    path
}   

## "resolve" clipping paths

resolvedClipPath <- function(path, index) {
    index <- as.integer(index)
    path$index <- index
    class(path) <- c("GridResolvedClipPath", class(path))
    path
}    

