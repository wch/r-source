ts.union <- function(..., dframe = FALSE) {
    makeNames <- function(...) {
        l <- as.list(substitute(list(...)))[-1]
        nm <- names(l)
        fixup <- if(is.null(nm)) seq(along = l) else nm == ""
        dep <- sapply(l[fixup], function(x) deparse(x)[1])
        if(is.null(nm)) return(dep)
        if(any(fixup)) nm[fixup] <- dep
        nm
    }
    .cbind.ts(list(...), makeNames(...), dframe = dframe, union = TRUE)
}

ts.intersect <- function(..., dframe = FALSE) {
    makeNames <- function(...) {
        l <- as.list(substitute(list(...)))[-1]
        nm <- names(l)
        fixup <- if(is.null(nm)) seq(along = l) else nm == ""
        dep <- sapply(l[fixup], function(x) deparse(x)[1])
        if(is.null(nm)) return(dep)
        if(any(fixup)) nm[fixup] <- dep
        nm
    }
    .cbind.ts(list(...), makeNames(...), dframe = dframe, union = FALSE)
}
