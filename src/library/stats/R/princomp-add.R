#  File src/library/stats/R/princomp-add.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2012 The R Core Team
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

predict.princomp <- function(object, newdata, ...)
{
    if (missing(newdata)) return(object$scores)
    if(length(dim(newdata)) != 2L)
        stop("'newdata' must be a matrix or data frame")
    p <- NCOL(object$loadings)
    nm <- rownames(object$loadings)
    if(!is.null(nm)) {
        if(!all(nm %in% colnames(newdata)))
            stop("'newdata' does not have named columns matching one or more of the original columns")
        newdata <- newdata[, nm]
    } else {
        if(NCOL(newdata) != p)
            stop("'newdata' does not have the correct number of columns")
    }
    ## next line does as.matrix
    scale(newdata, object$center, object$scale) %*% object$loadings
}

summary.princomp <- function(object, loadings = FALSE, cutoff = 0.1, ...)
{
    object$cutoff <- cutoff
    object$print.loadings <- loadings
    class(object) <- "summary.princomp"
    object
}

print.summary.princomp <-
    function(x, digits = 3L, loadings = x$print.loadings, cutoff = x$cutoff,
             ...)
{
    vars <- x$sdev^2
    vars <- vars/sum(vars)
    cat("Importance of components:\n")
    print(rbind("Standard deviation" = x$sdev,
                "Proportion of Variance" = vars,
                "Cumulative Proportion" = cumsum(vars)))
    if(loadings) {
        cat("\nLoadings:\n")
        cx <- format(round(x$loadings, digits = digits))
        cx[abs(x$loadings) < cutoff] <-
            paste(rep(" ", nchar(cx[1,1], type="w")), collapse="")
        print(cx, quote = FALSE, ...)
    }
    invisible(x)
}

plot.princomp <- function(x, main = deparse(substitute(x)), ...)
  screeplot.default(x, main = main, ...)

screeplot <- function(x, ...) UseMethod("screeplot")

screeplot.default <-
function(x, npcs = min(10, length(x$sdev)),
         type = c("barplot", "lines"),
         main = deparse(substitute(x)), ...)
{
    main
    type <- match.arg(type)
    pcs <- x$sdev^2
    xp <- seq_len(npcs)
    dev.hold(); on.exit(dev.flush())
    if(type == "barplot")
        barplot(pcs[xp], names.arg = names(pcs[xp]), main = main,
                ylab = "Variances", ...)
    else {
        plot(xp, pcs[xp], type = "b", axes = FALSE, main = main,
             xlab = "", ylab = "Variances", ...)
        axis(2)
        axis(1, at = xp, labels = names(pcs[xp]))
    }
    invisible()
}

loadings <- function(x) x$loadings
