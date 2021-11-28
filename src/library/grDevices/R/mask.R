
maskTypes <- c("alpha", "luminance")

.typeIndex <- function(x) {
    type <- match(x, maskTypes)
    if (is.na(type))
        stop("Invalid mask type")
    as.integer(type)
}

.mask <- function(fun, type) {
    attr(fun, "type") <- .typeIndex(type)
    fun
}

.setMask <- function(mask, ref) {    
    .External(C_setMask, mask, ref)
}
