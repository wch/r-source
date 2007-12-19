#  File src/library/base/R/by.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

by <- function(data, INDICES, FUN, ...) UseMethod("by")

## prior to 2.7.0 this promoted vectors to data frames, but
## the data frame method dropped to a single column.
by.default <- function(data, INDICES, FUN, ...)
{
    dd <- as.data.frame(data)
    if(length(dim(data)))
        by(dd, INDICES, FUN, ...)
    else {
        if(!is.list(INDICES)) {        # record the names for print.by
            IND <- vector("list", 1)
            IND[[1]] <- INDICES
            names(IND) <- deparse(substitute(INDICES))[1]
        } else IND <- INDICES
        FUNx <- function(x) FUN(dd[x,], ...)
        nd <- nrow(dd)
        ans <- eval(substitute(tapply(1:nd, IND, FUNx)), dd)
        attr(ans, "call") <- match.call()
        class(ans) <- "by"
        ans
    }
}

by.data.frame <- function(data, INDICES, FUN, ...)
{
    if(!is.list(INDICES)) { # record the names for print.by
        IND <- vector("list", 1)
        IND[[1]] <- INDICES
        names(IND) <- deparse(substitute(INDICES))[1]
    } else IND <- INDICES
    FUNx <- function(x) FUN(data[x,, drop=FALSE], ...) # (PR#10506)
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
