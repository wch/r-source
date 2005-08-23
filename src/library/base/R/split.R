split <- function(x, f, drop = FALSE, ...) UseMethod("split")

split.default <- function(x, f, drop = FALSE, ...)
{
    if(length(list(...))) .NotYetUsed(deparse(...), error = FALSE)

    if (is.list(f)) f <- interaction(f, drop = drop)
    else if (drop || !is.factor(f)) # drop extraneous levels
	f <- factor(f)
    if (is.null(attr(x, "class")))
	return(.Internal(split(x, f)))
    ## else
    lf <- levels(f)
    y <- vector("list", length(lf))
    names(y) <- lf
    for(k in lf) y[[k]] <- x[f %in% k]
    y
}

split.data.frame <- function(x, f, drop = FALSE, ...)
    lapply(split(seq(length=nrow(x)), f, drop = drop, ...),
           function(ind) x[ind, , drop = FALSE])

"split<-" <- function(x, f, drop = FALSE, ..., value) UseMethod("split<-")

#"split<-.default" <- function(x, f, value)
#{
#    x[unlist(split(seq(along=x), f))] <- unlist(value)
#    x
#}

"split<-.default" <- function(x, f, drop = FALSE, ..., value)
{
    ix <- split(seq(along = x), f, drop = drop, ...)
    n <- length(value)
    j <- 0
    for (i in ix) {
        j <- j %% n + 1
        x[i] <- value[[j]]
    }
    x
}


#"split<-.data.frame" <- function(x, f, value)
#{
#    x[unlist(split(seq(length=nrow(x)), f)),] <- do.call("rbind", value)
#    x
#}

"split<-.data.frame" <- function(x, f, drop = FALSE, ..., value)
{
    ix <- split(seq(along = x), f, drop = drop, ...)
    n <- length(value)
    j <- 0
    for (i in ix) {
        j <- j %% n + 1
        x[i,] <- value[[j]]
    }
    x
}

unsplit <- function(value, f, drop = FALSE)
{
    len <- length(if (is.list(f)) f[[1]] else f)
    x <- vector(mode = typeof(value[[1]]), length = len)
    split(x, f, drop = drop) <- value
    x
}
