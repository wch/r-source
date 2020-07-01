
createMask <- function(mask) {
    force(mask)
    maskFun <- function() {
        grid.draw(mask, recording=FALSE)
    }
    result <- list(f=maskFun, ref=NULL)
    class(result) <- "GridMask"
    result
}   

isMask <- function(x) {
    inherits(x, "GridMask")
}

## "resolve" masks
resolveMask <- function(path) {
    UseMethod("resolveMask")
}

resolveMask.NULL <- function(mask) {
    .setMask(NULL, NULL)
    NULL
}

resolveMask.GridMask <- function(mask) {
    ref <- .setMask(mask$f, mask$ref)
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

unresolveMask <- function(mask) {
    UseMethod("unresolveMask")
}
    
## Unresolved masks just pass through
unresolveMask.GridMask <- function(mask) {
    mask
}

unresolveMask.GridResolvedMask <- function(mask) {
    result <- list(f=mask$f, ref=NULL)
    class(result) <- "GridMask"
    result
}

