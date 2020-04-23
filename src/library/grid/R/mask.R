
createMask <- function(mask) {
    force(mask)
    maskFun <- function() {
        grid.draw(mask, recording=FALSE)
    }
    result <- list(f=maskFun, ref=NULL)
    class(result) <- "GridMask"
    result
}   

## "resolve" masks
resolveMask <- function(path) {
    UseMethod("resolveMask")
}

resolveMask.NULL <- function(mask) {
    grDevices::setMask(NULL, -1)
    NULL
}

resolveMask.GridMask <- function(mask) {
    ref <- grDevices::setMask(mask$f, mask$ref)
    resolvedMask(mask, ref)
}

resolvedMask <- function(mask, ref) {
    UseMethod("resolvedMask")
}

resolvedMask.GridMask <- function(mask, ref) {
    mask$ref <- ref
    class(mask) <- c("GridResolvedMask", class(mask))
    mask
}    

resolvedMask.GridResolvedMask <- function(mask, ref) {
    mask$ref <- ref
    mask
}    


