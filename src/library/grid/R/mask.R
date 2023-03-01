
createMask <- function(mask, type="alpha") {
    force(mask)
    maskFun <- function() {
        grid.draw(mask, recording=FALSE)
    }
    result <- list(f=.mask(maskFun, type), ref=NULL)
    class(result) <- "GridMask"
    result
}   

isMask <- function(x) {
    inherits(x, "GridMask")
}

## "resolve" masks
resolveMask <- function(mask) {
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

## User interface
as.mask <- function(x, type=c("alpha", "luminance")) {
    if (!is.grob(x))
        stop("Only a grob can be converted to a mask")
    createMask(x, match.arg(type))
}
