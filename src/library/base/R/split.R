split <- function(x, f) UseMethod("split")

split.default <- function(x, f) {
    if (is.list(f))
        f <- factor(do.call("interaction", f))
    else
        f <- factor(f)                  # drop extraneous levels
    if (is.null(attr(x, "class")) && is.null(names(x)))
        return(.Internal(split(x, f)))
    ## else
    lf <- levels(f)
    y <- vector("list", length(lf))
    names(y) <- lf
    for(k in lf){
        y[[k]] <- x[f==k]
    }
    y
}

split.data.frame <- function(x, f) {
    lapply(split(seq(length=nrow(x)), f), function(ind) x[ind, , drop = FALSE ])
}

"split<-" <- function(x, f, value) UseMethod("split<-")

"split<-.default" <- function(x, f, value){
    x[unlist(split(seq(along=x), f))] <- unlist(value)
    x
}

"split.data.frame<-" <- "split<-.data.frame" <- function(x, f, value){
    x[unlist(split(seq(length=nrow(x)), f)),] <- do.call("rbind", value)
    x
}

unsplit <- function(value, f) {
    x <- vector(mode=typeof(value[[1]]), length=length(f))
    split(x, f) <- value
    x
}
