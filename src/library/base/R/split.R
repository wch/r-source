split <- function(x, f) UseMethod("split")

split.default <- function(x, f)
{
    if (is.list(f)) f <- interaction(f)
    f <- factor(f)                  # drop extraneous levels
    if (is.null(attr(x, "class")))
        return(.Internal(split(x, f)))
    ## else
    lf <- levels(f)
    y <- vector("list", length(lf))
    names(y) <- lf
    for(k in lf) y[[k]] <- x[f %in% k]
    y
}

split.data.frame <- function(x, f)
    lapply(split(seq(length=nrow(x)), f), function(ind) x[ind, , drop = FALSE ])

"split<-" <- function(x, f, value) UseMethod("split<-")

#"split<-.default" <- function(x, f, value)
#{
#    x[unlist(plit(seq(along=x), f))] <- unlist(value)
#    x
#}

"split<-.default" <- function(x, f, value)
{
    ix <- split(seq(along = x), f)
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

"split<-.data.frame" <- function(x, f, value)
{
    ix <- split(seq(along = x), f)
    n <- length(value)
    j <- 0
    for (i in ix) {
        j <- j %% n + 1
        x[i,] <- value[[j]]
    }
    x
}

unsplit <- function(value, f)
{
    len <- length(if (is.list(f)) f[[1]] else f)
    x <- vector(mode = typeof(value[[1]]), length = len)
    split(x, f) <- value
    x
}
