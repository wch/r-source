by <- function(data, INDICES, FUN, ...) UseMethod("by")

by.default <- function(data, INDICES, FUN, ...)
    by(as.data.frame(data), INDICES, FUN, ...)

by.data.frame <- function(data, INDICES, FUN, ...)
{
    if(!is.list(INDICES)) { # record the names for print.by
        IND <- vector("list", 1)
        IND[[1]] <- INDICES
        names(IND) <- deparse(substitute(INDICES))[1]
    } else IND <- INDICES
    FUNx <- function(x) FUN(data[x,], ...)
    nd <- nrow(data)
    ans <- eval(substitute(tapply(1:nd, IND, FUNx)), data)
    attr(ans, "call") <- match.call()
    class(ans) <- "by"
    ans
}

print.by <- function(x, ..., vsep)
{
    d <- dim(x)
    dn <- dimnames(x)
    dnn <- names(dn)
    if(missing(vsep))
        vsep <- paste(rep("-", 0.75*getOption("width")), collapse = "")
    lapply(seq_along(x), function(i, x, vsep, ...) {
        if(i != 1 && !is.null(vsep)) cat(vsep, "\n")
        ii <- i - 1
        for(j in seq_along(dn)) {
            iii <- ii %% d[j] + 1; ii <- ii %/% d[j]
            cat(dnn[j], ": ", dn[[j]][iii], "\n", sep = "")
        }
        print(x[[i]], ...)
    } , x, vsep, ...)
    invisible(x)
}
