split <- function(x, f) UseMethod("split")

split.default <- function(x, f) {
    if(is.list(f))
        f <- factor(do.call("interaction", f))
    else
        f <- factor(f)                  # drop extraneous levels
    if(is.null(attr(x, "class")) && is.null(names(x)))
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
    lapply(split(1:nrow(x), f), function(ind) x[ind, , drop = FALSE ])
}
