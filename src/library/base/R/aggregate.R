aggregate <- function(x, ...) UseMethod("aggregate")

aggregate.default <- function(x, ...) {
    if (is.ts(x))
        aggregate.ts(as.ts(x), ...)
    else
        aggregate.data.frame(as.data.frame(x), ...)
}

aggregate.data.frame <- function(x, by, FUN, ...) {
    if (!is.data.frame(x))
        x <- as.data.frame(x)
    if (!is.list(by))
        stop("`by' must be a list")
    if (is.null(names(by)))
        names(by) <- paste("Group", seq(along = by), sep = ".")
    else {
        nam <- names(by)
        ind <- which(nchar(nam) == 0)
        names(by)[ind] <- paste("Group", ind, sep = ".")
    }
    y <- lapply(x, tapply, by, FUN, ..., simplify = FALSE)
    if (any(sapply(unlist(y, recursive = FALSE), length) > 1))
        stop("`FUN' must always return a scalar")
    z <- y[[1]]
    d <- dim(z)
    w <- NULL    
    for (i in seq(along = d)) {
        j <- rep(rep(seq(1 : d[i]),
                     prod(d[seq(length = i - 1)]) * rep(1, d[i])),
                 prod(d[seq(from = i + 1, length = length(d) - i)]))
        w <- cbind(w, dimnames(z)[[i]][j])
    }
    w <- w[which(!unlist(lapply(z, is.null))), ]
    y <- data.frame(w, lapply(y, unlist, use.names = FALSE))
    names(y) <- c(names(by), names(x))
    y
}

aggregate.ts <- function(x, nfrequency = 1, FUN = sum, ndeltat = 1) {
    .NotYetImplemented()
}
