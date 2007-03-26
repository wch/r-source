unname <- function (obj, force= FALSE) {
    if (length(names(obj)))
        names(obj) <- NULL
    if (length(dimnames(obj)) && (force || !is.data.frame(obj)))
        dimnames(obj) <- NULL
    obj
}
