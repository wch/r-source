merge <- function(x, y, ...) UseMethod("merge")

merge.default <- function(x, y, ...)
    merge(as.data.frame(x), as.data.frame(y), ...)

merge.data.frame <-
    function(x, y, by = intersect(names(x), names(y)),
             by.x = by, by.y = by, sort = TRUE)
{
    fix.by <- function(by, df)
    {
        ## fix up `by' to be a valid set of cols by number: 0 is row.names
        by <- as.vector(by)
        nc <- ncol(df)
        if(is.character(by))
            by <- match(by, c("row.names", names(df))) - 1
        else if(is.numeric(by)) {
            if(!all(0 <= by <= nc)) stop("`by' must match numbers of columns")
        } else if(is.logical(by)) {
            if(length(by) != nc) stop("`by' must match number of columns")
            by <- seq(along = by)[by]
        } else stop("`by' must specify column(s)")
        if(any(is.na(by))) stop("`by' must specify valid column(s)")
        unique(by)
    }

    x <- as.data.frame(x); y <- as.data.frame(y)
    if (nrow(x) == 0 || nrow(y) == 0)
        stop("no rows to match")
    by.x <- fix.by(by.x, x); by.y <- fix.by(by.y, y)
    if(length(by.x) && any(by.x == 0)) {
        x <- cbind(Row.names = row.names(x), x)
        by.x <- by.x + 1
    }
    if(any(by.y == 0)) {
        y <- cbind(Row.names = row.names(y), y)
        by.y <- by.y + 1
    }
    if(length(by.x) == 0) stop("no columns to match on")
    if(length(by.x) != length(by.y))
        stop("by.x and by.y specify different numbers of columns")
    row.names(x) <- 1:nrow(x)
    row.names(y) <- 1:nrow(y)
    ## create keys from by cols.
    bx <- matrix(as.character(as.matrix.data.frame(x[, by.x, drop=FALSE])),
                 nrow(x))
    bx <- drop(apply(bx, 1, function(x) paste(x, collapse="\r")))
    by <- matrix(as.character(as.matrix.data.frame(y[, by.y, drop=FALSE])),
                 nrow(y))
    by <- drop(apply(by, 1, function(x) paste(x, collapse="\r")))
    comm <- match(bx, by, 0)
    bxy <- bx[comm > 0]
    xinds <- match(bx, bxy, 0)
    yinds <- match(by, bxy, 0)
#    o <- outer(xinds, yinds, function(x, y) (x > 0) & x==y)
#    xi <- row(o)[o]
#    yi <- col(o)[o]
    m <- .Internal(merge(xinds, yinds))
    nm <- nm.x <- names(x)[-by.x]
    nm.y <- names(y)[-by.y]
    cnm <- match(nm.x, nm.y, 0)
    nm.x[cnm > 0] <- paste(nm.x[cnm > 0], "x", sep=".")
    x <- x[m$xi, c(by.x, seq(length=ncol(x))[-by.x]), drop=FALSE]
    names(x) <- c(names(x)[seq(along=by.x)], nm.x)
    cnm <- match(nm.y, nm, 0)
    nm.y[cnm > 0] <- paste(nm.y[cnm > 0], "y", sep=".")
    y <- y[m$yi, -by.y, drop=FALSE]
    names(y) <- nm.y
    res <- cbind(x, y)
    if (sort) res  <- res[sort.list(bx[m$xi]),, drop=FALSE]
    row.names(res) <- seq(length=nrow(res))
    res
}
